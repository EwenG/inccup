(ns ewen.inccup.incremental.compiler-macros
  (:require [ewen.inccup.incremental.compiler :as comp]
            [ewen.inccup.incremental.emitter :as emitter]
            [cljs.analyzer.api :as ana-api]
            [clojure.walk :refer [postwalk]]
            [clojure.set :refer [union]]))

(def ^:dynamic *cache-sym* nil)
(def ^:dynamic *cache-static-counter* nil)
(def ^:dynamic *params-changed-sym* nil)
(def ^:dynamic *dynamic-forms* nil)
(def ^:dynamic *tracked-vars* #{})
(def ^:dynamic *env* nil)

(defn new-local-binding [name env]
  {:name name
   :binding-form? true
   :op :var
   :env env
   :info {:name name, :shadow nil}
   :shadow nil
   :local true})

(defn maybe-add-env-local [{:keys [locals] :as env} sym]
  (if (get locals sym)
    env
    (update-in env [:locals]
               assoc sym
               (new-local-binding sym env))))

(defn track-vars [expr]
  (binding [*tracked-vars* *tracked-vars*]
    (let [{:keys [locals] :as env} (or *env* (ana-api/empty-env))
          ;; Add *cache-sym* and all *params-changed-sym* to the
          ;; environment local bindings of the analyzer if it is not
          ;; already there. This is useful because the macro-expansion
          ;; order is inverted by the call to the analyzer and emitter
          ;; and thus, the wrapping let forms are not analyzed.
          env (reduce maybe-add-env-local
                      (or *env* (ana-api/empty-env))
                      (into [*cache-sym*] (vals *params-changed-sym*)))
          cljs-expanded (-> env (ana-api/analyze expr) emitter/emit*)
          used-vars (keep #(when (:is-used %) (:symbol %))
                          (vals *tracked-vars*))]
      [cljs-expanded (set used-vars)])))

(defn update-dynamic-forms [expr path used-vars]
  (when *dynamic-forms*
    (let [{prev-path :path
           prev-var-deps :var-deps
           prev-form :form} (peek *dynamic-forms*)]
      (if (= path prev-path)
        (set! *dynamic-forms*
              (conj (pop *dynamic-forms*)
                    {:path path
                     :var-deps (union used-vars prev-var-deps)
                     :form `(comp ~expr ~prev-form)}))
        (set! *dynamic-forms*
              (conj *dynamic-forms*
                    {:path path
                     :var-deps used-vars
                     :form expr}))))))

(defn compile-dynamic-expr [expr path]
  (let [[expr used-vars] (track-vars expr)]
    (update-dynamic-forms `(constantly ~expr) path used-vars)
    nil))

(defn compile-attr-map
  "Returns an unevaluated form that will render the supplied map as HTML
  attributes."
  [attrs path]
  (if (some comp/unevaluated? (mapcat identity attrs))
    (compile-dynamic-expr attrs path)
    attrs))

(defn maybe-attr-map
  ([attrs attr-path]
   (maybe-attr-map attrs attr-path nil))
  ([attrs attr-path path]
   {:pred [(not (some comp/unevaluated? (mapcat identity attrs)))]}
   (when attrs
     (let [[expr used-vars] (track-vars attrs)
           attr-form `#(if (map? ~expr)
                         (comp/merge-map-attributes % ~expr)
                         (comp/merge-map-attributes % {}))
           form (when path `#(if (map? ~expr) nil ~expr))]
       (update-dynamic-forms attr-form attr-path used-vars)
       (when expr (update-dynamic-forms form path used-vars))
       nil))))

(defn- literal?
  "True if x is a literal value that can be rendered as-is."
  [x]
  (and (not (comp/unevaluated? x))
       (or (not (or (vector? x) (map? x)))
           (every? literal? x))))

(defn dynamic-tag [tag tag-path attr-path]
  {:pred [(not (literal? tag))]}
  (let [[expr used-vars] (track-vars tag)
        tag-form `#(let [[tag# ~'_ ~'_] (comp/normalize-element [~expr])]
                     tag#)
        attr-form `#(let [[~'_ tag-attrs# _]
                          (comp/normalize-element [~expr])]
                      (comp/merge-shortcut-attributes % tag-attrs#))]
    (update-dynamic-forms tag-form tag-path used-vars)
    (update-dynamic-forms attr-form attr-path used-vars)
    nil))

(defn- form-name
  "Get the name of the supplied form."
  [form]
  (if (and (seq? form) (symbol? (first form)))
    (name (first form))))

(declare compile-html)

(defmulti compile-form
  "Pre-compile certain standard forms, where possible."
  {:private true}
  (fn [expr path] (form-name expr)))

(defmethod compile-form "for"
  [[_ bindings body :as expr] path]
  ;; If the body of the for expression is not at least partially a literal,
  ;; it is useless to try to compile it
  (let [body (if (or (vector? body) (literal? body))
               `(compile-inc ~body) body)]
    (compile-dynamic-expr `(for ~bindings ~body) path)))

(defmethod compile-form "if"
  [[_ condition & body] path]
  `(if ~condition
     ~@(for [x body]
         ;; Do not try to compile any expression of the if body that is not
         ;; at least partially a literal
         (if (or (vector? x) (literal? x))
           `(compile-inc ~x) x))))

(defmethod compile-form :default
  [expr path] (compile-dynamic-expr expr path))

(defn- not-hint?
  "True if x is not hinted to be the supplied type."
  [x type]
  (if-let [hint (-> x meta :tag)]
    (not (isa? (eval hint) type))))

(defn- hint?
  "True if x is hinted to be the supplied type."
  [x type]
  (if-let [hint (-> x meta :tag)]
    (isa? (eval hint) type)))

(defn- not-implicit-map?
  "True if we can infer that x is not a map."
  [x]
  (or (= (form-name x) "for")
      (not (comp/unevaluated? x))
      (not-hint? x java.util.Map)))

(defn with-dom-node [content]
  `(with-meta ~content (cljs.core/js-obj "dom-node" nil)))

(defn- element-compile-strategy
  "Returns the compilation strategy to use for a given element."
  [[tag attrs & content :as element] path]
  (cond
    (every? literal? element)
    ::all-literal                    ; e.g. [:span "foo"]
    (and (literal? tag) (map? attrs))
    ::literal-tag-and-attributes     ; e.g. [:span {} x]
    (and (literal? tag) (not-implicit-map? attrs))
    ::literal-tag-and-no-attributes  ; e.g. [:span ^String x]
    (literal? tag)
    ::literal-tag                    ; e.g. [:span x]
    :else
    ::default))

(declare compile-seq)

(defmulti compile-element
  {:private true}
  element-compile-strategy)

(defmethod compile-element ::all-literal
  [element path] (with-dom-node element))

(defmethod compile-element ::literal-tag-and-attributes
  [[tag attrs & content] path]
  (let [[tag attrs _] (comp/normalize-element [tag attrs])
        compiled-attrs (compile-attr-map attrs (conj path 1))]
    (with-dom-node
      (into [tag compiled-attrs] (compile-seq content path 2)))))

(defmethod compile-element ::literal-tag-and-no-attributes
  [[tag & content] path]
  (compile-element (apply vector tag {} content) path))

(defmethod compile-element ::literal-tag
  [[tag attrs & content :as element] path]
  (let [[tag tag-attrs [first-content & rest-content]]
        (comp/normalize-element element)
        tag-attrs (if (map? tag-attrs)
                    (vary-meta
                     tag-attrs assoc ::comp/shortcut-attrs tag-attrs)
                    tag-attrs)]
    (maybe-attr-map first-content (conj path 1) (conj path 2))
    (with-dom-node
      (into [tag tag-attrs nil] (compile-seq rest-content path 3)))))

(defmethod compile-element ::default
  [[tag attrs & rest-content] path]
  (dynamic-tag tag (conj path 0) (conj path 1))
  (maybe-attr-map attrs (conj path 1) (conj path 2))
  (if (nil? attrs)
    [nil {}]
    (with-dom-node
      (into [nil {} nil] (compile-seq rest-content path 3)))))

(declare compile-dispatch)

(defn- compile-seq
  "Compile a sequence of data-structures into HTML."
  ([content path]
   (compile-seq content path 0))
  ([content path index-init]
   (loop [[expr & rest-content] content
          index index-init
          compiled []]
     (if expr
       (let [compiled-expr (compile-dispatch expr (conj path index))]
         (recur rest-content (inc index) (conj compiled compiled-expr)))
       compiled))))

(defn- compile-dispatch [expr path]
  (cond
    (vector? expr) (compile-element expr path)
    (string? expr) expr
    (keyword? expr) expr
    (literal? expr) expr
    (seq? expr) (compile-form expr path)
    :else (compile-dynamic-expr expr path)))

(defn compute-common-path [dynamic-forms]
  (let [var-deps (apply union (map :var-deps dynamic-forms))
        path-groups (->> (map :path dynamic-forms)
                         (apply interleave)
                         (partition (count dynamic-forms)))
        common-path (loop [[path-group & rest-path-groups] path-groups
                           common-path []]
                      (if (or (nil? path-group) (not (apply = path-group)))
                        common-path
                        (recur rest-path-groups
                               (conj common-path (first path-group)))))]
    [common-path var-deps]))

(defn remove-from-path [n form]
  {:post [(-> (:path %) empty? not)]}
  (update form :path subvec n))

(defn group-dynamic-forms [dynamic-forms]
  {:pre [(not (empty? dynamic-forms))]}
  (let [partitioned (partition-by (comp first :path) dynamic-forms)]
    (for [forms partitioned]
      (if (= 1 (count forms))
        (first forms)
        (let [[common-path var-deps] (compute-common-path forms)
              n (count common-path)
              updated-forms (map (partial remove-from-path n) forms)]
          {:path common-path :var-deps var-deps
           :sub-forms (group-dynamic-forms updated-forms)})))))

(comment
  (compute-common-path [{:path [2 0 0]
                         :var-deps #{1}}
                        {:path [2 0 1 0 0]
                         :var-deps #{1 2}}
                        {:path [2 0 1 0 1]
                         :var-deps #{1 2}}])

  (group-dynamic-forms '[{:path [0 1]
                          :var-deps #{x}
                          :form (constantly x)}
                         {:path [2 0 0]
                          :var-deps #{x}
                          :form (constantly x)}
                         {:path [2 0 1 0 0]
                          :var-deps #{x y}
                          :form #(update-in % (constantly y))}
                         {:path [2 0 1 0 1]
                          :var-deps #{z}
                          :form (constantly z)}])

  ;; Should throw an error
  (group-dynamic-forms '[{:path [0 1] :var-deps #{x} :form (constantly x)}
                         {:path [2 0 1] :var-deps #{x} :form (constantly x)}
                         {:path [2 0 1 0 0]
                          :var-deps #{x y}
                          :form (constantly y)}
                         {:path [2 0 1 0 1]
                          :var-deps #{z}
                          :form (constantly z)}])
  )

(declare dynamic-forms->update-expr)

(defn var-deps->predicate [var-deps]
  (cond (nil? *params-changed-sym*) true
        (empty? var-deps) `(not (comp/safe-aget ~*cache-sym* "params"))
        :else (let [preds (doall
                           (map #(get *params-changed-sym* %) var-deps))]
                (if (= 1 (count preds))
                  (first preds)
                  `(or ~@preds)))))

(defn dynamic-form->update-expr [{:keys [path var-deps form sub-forms]}]
  {:pre [(or (and form (nil? sub-forms))
             (and (nil? form) sub-forms))]}
  (let [predicate (var-deps->predicate var-deps)]
    (cond
      (empty? path) ;; only dynamic
      `(~(var-deps->predicate var-deps) (~form))
      form
      `(~(var-deps->predicate var-deps)
        (update-in ~path ~form))
      :else
      `(~(var-deps->predicate var-deps)
        (update-in ~path ~(dynamic-forms->update-expr sub-forms))))))

(defn dynamic-forms->update-expr [dynamic-forms]
  (if (empty? dynamic-forms) ;; only static
    `identity
    (let [update-exprs (mapcat dynamic-form->update-expr dynamic-forms)]
      `(fn [form#]
         (cond-> form# ~@update-exprs)))))

(comment
  (binding [*params-changed-sym* {'x 'x123 'y 'y234 'z 'z456}]
    (dynamic-forms->update-expr
     '({:path [0 1] :var-deps #{x} :form (constantly x)}
       {:path [2 0]
        :var-deps #{x y z}
        :sub-forms
        ({:path [0] :var-deps #{x} :form (constantly x)}
         {:path [1 0]
          :var-deps #{x y z}
          :sub-forms
          ({:path [0] :var-deps #{x y} :form (constantly y)}
           {:path [1] :var-deps #{z} :form (constantly z)})})})))
  )

(defn extract-params [params]
  (let [extracted (atom #{})]
    (postwalk (fn [x]
                (when (and (not= '& x) (symbol? x))
                  (swap! extracted conj x))
                x)
              params)
    @extracted))

(comment
  (extract-params '[{:e r :as rr} y & others])
  )

(defn compile-inc* [content env]
  (let [[static dynamic]
        (binding [*env* env
                  *dynamic-forms* []]
          [(compile-dispatch content []) *dynamic-forms*])
        update-expr (dynamic-forms->update-expr dynamic)]
    [static update-expr]))

(defn with-params-changed [params params-changed-sym & body]
  (let [changed-bindings
        (mapcat (fn [param]
                  [(get params-changed-sym param)
                   `(or (not (comp/safe-aget ~*cache-sym* "params"))
                        (not= ~param
                              (get (comp/safe-aget ~*cache-sym* "params")
                                   '~param)))])
                params)]
    `(let ~(vec changed-bindings) ~@body)))

(defmacro compile-inc [content]
  (if *cache-static-counter*
    ;; The html macro is used inside a defhtml
    (do (set! *cache-static-counter* (inc *cache-static-counter*))
        (let [static-counter (dec *cache-static-counter*)
              [static update-expr] (compile-inc* content &env)]
          `(let [~*cache-sym* (comp/get-static-cache
                               ~*cache-sym* ~static-counter)
                 ~*cache-sym* (comp/new-dynamic-cache ~*cache-sym*)]
             (let [result# (-> (comp/safe-aget ~*cache-sym* "prev-result")
                               (or ~static)
                               (~update-expr))]
               (comp/safe-aset ~*cache-sym* "prev-result" result#)
               result#))))
    ;; The html macro is used outside a defhtml, there is no need for
    ;; caching
    content))

(defmacro compile-inc-with-params [content params]
   (let [params-with-sym (interleave (map #(list 'quote %) params) params)
         params-changed-sym (zipmap params (map #(gensym (name %)) params))
         tracked-vars
         (loop [tracked-vars {}
                params params]
           (if-let [param (first params)]
             (do (assert (contains? (:locals &env) param))
                 (recur (assoc tracked-vars param
                               {:env (get (:locals &env) param)
                                :is-used false
                                :symbol param})
                        (rest params)))
             tracked-vars))]
     (binding [*tracked-vars* tracked-vars
               *params-changed-sym* params-changed-sym
               *cache-static-counter* 0
               *cache-sym* (gensym "cache")]
       (let [[static update-expr] (compile-inc* content &env)]
         ;; The cache-sym is bound lexically and not dynamically, otherwise
         ;; it could be an issue in the presence of lazy evaluation because
         ;; lazy evaluation and dynamic binding don't play well together
         `(let [~*cache-sym* (comp/new-dynamic-cache comp/*cache*)]
            ~(with-params-changed params params-changed-sym
               `(comp/make-static-cache
                 ~*cache-sym* ~*cache-static-counter*)
               `(let [result# (-> (comp/safe-aget
                                   ~*cache-sym* "prev-result")
                                  (or ~static)
                                  (~update-expr))]
                  (comp/safe-aset
                   ~*cache-sym* "params" ~(conj params-with-sym `hash-map))
                  (comp/safe-aset ~*cache-sym* "prev-result" result#)
                  (comp/clean-sub-cache ~*cache-sym*)
                  result#)))))))

(defn collect-input-var-deps
  [tracked-vars collected-vars
   {:keys [op local init info env children] :as ast}]
  (let [name (:name info)]
    (cond
      (and (= :var op)
           (contains? tracked-vars name)
           (identical? (get (:locals env) name)
                       (get-in tracked-vars [name :env])))
      (conj collected-vars ast)
      (and (= :var op) local init)
      (collect-input-var-deps tracked-vars collected-vars init)
      (not (empty? children))
      (->> (map (partial collect-input-var-deps tracked-vars collected-vars)
                children)
           (apply union))
      :else collected-vars)))

(defmethod emitter/emit* :var
  [{:keys [info env form] :as ast}]
  (let [used-vars (collect-input-var-deps *tracked-vars* #{} ast)]
    (doseq [{{name :name} :info} used-vars]
      (set! *tracked-vars*
            (assoc-in *tracked-vars* [name :is-used] true))))
  (emitter/var-emit ast))

(defmethod emitter/emit* :invoke
  [{:keys [f args] :as ast}]
  (let [is-defhtml? (-> (:info f) :meta :ewen.inccup.core/defhtml)]
    (if is-defhtml?
      (do
        (set! *cache-static-counter* (inc *cache-static-counter*))
        `(binding [comp/*cache* (comp/get-static-cache
                                 ~*cache-sym*
                                 ~(dec *cache-static-counter*))]
           ~(emitter/invoke-emit ast)))
      (emitter/invoke-emit ast))))

(comment
  (binding [*tracked-vars* {'e false}]
    (emit* (ana-api/analyze (ana-api/empty-env)
                            '(let [e "e"]
                               e)))
    *tracked-vars*)
  )

(comment
  (require '[clojure.pprint :refer [pprint pp]])

  (binding [*dynamic-forms* #{}]
    (compile-element '[:e {} [:p {} x]] []))

  (binding [*cache-static-counter* 0]
    (let [x "c"]
      (compile-inc '[:e {} [:p {} x]])))
 )
