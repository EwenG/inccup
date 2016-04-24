(ns ewen.inccup.incremental.compiler-macros
  (:require [ewen.inccup.incremental.emitter :as emitter]
            [ewen.inccup.util :as util]
            [cljs.analyzer.api :as ana-api]
            [clojure.walk :refer [postwalk]]
            [clojure.set :refer [union]]))

(def ^:dynamic *cache-sym* nil)
(def ^:dynamic *cache-static-counter* nil)
(def ^:dynamic *params-changed-sym* nil)
(def ^:dynamic *dynamic-forms* nil)
(def ^:dynamic *tracked-vars* #{})
(def ^:dynamic *env* nil)
(def ^:dynamic *update-paths* [])
(def ^:dynamic *skips* [])
(def ^:dynamic *statics* [])

(defn new-local-binding [name env]
  {:name name
   :binding-form? true
   :op :var
   :env env
   :info {:name name, :shadow nil}
   :shadow nil
   :local true})

(defn add-local-in-env [{:keys [locals] :as env} sym]
  (if sym
    (update-in env [:locals]
               assoc sym
               (new-local-binding sym env))
    env))

(defn track-vars [expr]
  (binding [*tracked-vars* *tracked-vars*]
    (let [{:keys [locals] :as env} (or *env* (ana-api/empty-env))
          ;; Add *cache-sym* and all *params-changed-sym* to the
          ;; environment local bindings of the analyzer if it is not
          ;; already there. This is useful because the macro-expansion
          ;; order is inverted by the call to the analyzer and emitter
          ;; and thus, the wrapping let forms are not analyzed.
          env (reduce add-local-in-env
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
    (update-dynamic-forms expr path used-vars)
    nil))

(defn compile-attr-map
  "Returns an unevaluated form that will render the supplied map as HTML
  attributes."
  [attrs path]
  (if (some util/unevaluated?
            (mapcat identity attrs))
    (compile-dynamic-expr attrs path)
    attrs))

(defn maybe-attr-map
  ([attrs attr-path path tag-attrs]
   {:pred [(not (some util/unevaluated? (mapcat identity attrs)))]}
   (when attrs
     (let [[expr used-vars] (track-vars attrs)
           attr-form
           `(ewen.inccup.incremental.compiler/maybe-merge-attributes
             ~tag-attrs ~expr)
           form `(if (map? ewen.inccup.incremental.compiler/*tmp-val*)
                   nil
                   ewen.inccup.incremental.compiler/*tmp-val*)]
       (update-dynamic-forms attr-form attr-path used-vars)
       (update-dynamic-forms form path used-vars)
       nil))))

(defn- literal?
  "True if x is a literal value that can be rendered as-is."
  [x]
  (and (not (util/unevaluated? x))
       (or (not (or (vector? x) (map? x)))
           (every? literal? x))))

(defn dynamic-tag [tag tag-path attr-path]
  {:pred [(not (literal? tag))]}
  (let [[expr used-vars] (track-vars tag)
        tag-form
        `(let [[tag# attrs#] (util/normalize-element [~expr])]
           (set! ewen.inccup.incremental.compiler/*tmp-val* attrs#)
           tag#)]
    (update-dynamic-forms tag-form tag-path used-vars)
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
      (not (util/unevaluated? x))
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
  (let [[tag attrs _] (util/normalize-element [tag attrs])
        compiled-attrs (compile-attr-map attrs (conj path 1))]
    (with-dom-node
      (into [tag compiled-attrs] (compile-seq content path 2)))))

(defmethod compile-element ::literal-tag-and-no-attributes
  [[tag & content] path]
  (compile-element (apply vector tag {} content) path))

(defmethod compile-element ::literal-tag
  [[tag attrs & content :as element] path]
  (let [[tag tag-attrs [first-content & rest-content]]
        (util/normalize-element element)]
    (maybe-attr-map first-content (conj path 1) (conj path 2) tag-attrs)
    (with-dom-node
      (into [tag tag-attrs nil] (compile-seq rest-content path 3)))))

(defmethod compile-element ::default
  [[tag attrs & rest-content] path]
  (dynamic-tag tag (conj path 0) (conj path 1))
  (maybe-attr-map attrs (conj path 1) (conj path 2) nil)
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
  (let [partitioned (partition-by (comp first :path) dynamic-forms)
        forms (for [forms partitioned]
                (if (= 1 (count forms))
                  (first forms)
                  (let [[common-path var-deps] (compute-common-path forms)
                        n (count common-path)
                        updated-forms (map
                                       (partial remove-from-path n)
                                       forms)]
                    {:path common-path :var-deps var-deps
                     :sub-forms (group-dynamic-forms updated-forms)})))]
    (if (> (count forms) 1)
      (let [var-deps (apply union (map :var-deps forms))]
        {:path [] :var-deps var-deps :sub-forms forms})
      (first forms))))

(comment
  (compute-common-path [{:path [2 0 0]
                         :var-deps #{1}}
                        {:path [2 0 1 0 0]
                         :var-deps #{1 2}}
                        {:path [2 0 1 0 1]
                         :var-deps #{1 2}}])

  (group-dynamic-forms '[{:path [2 0 0]
                          :var-deps #{x}
                          :form x}
                         {:path [2 0 1 0 0]
                          :var-deps #{x y}
                          :form #(update-in % y)}
                         {:path [2 0 1 0 1]
                          :var-deps #{z}
                          :form z}])

  (group-dynamic-forms '[{:path [0 1]
                          :var-deps #{x}
                          :form x}
                         {:path [2 0 0]
                          :var-deps #{x}
                          :form x}
                         {:path [2 0 1 0 0]
                          :var-deps #{x y}
                          :form #(update-in % y)}
                         {:path [2 0 1 0 1]
                          :var-deps #{z}
                          :form z}])

  ;; Should throw an error
  (group-dynamic-forms '[{:path [0 1] :var-deps #{x} :form x}
                         {:path [2 0 1] :var-deps #{x} :form x}
                         {:path [2 0 1 0 0]
                          :var-deps #{x y}
                          :form y}
                         {:path [2 0 1 0 1]
                          :var-deps #{z}
                          :form z}])
  )

(declare dynamic-forms->update-expr)

(defn var-deps->predicate [var-deps]
  (cond (nil? *params-changed-sym*) true
        (empty? var-deps) `(not (ewen.inccup.incremental.compiler/safe-aget
                                 ~*cache-sym* "params"))
        :else (let [preds (doall
                           (map #(get *params-changed-sym* %) var-deps))]
                (if (= 1 (count preds))
                  (first preds)
                  `(or ~@preds)))))

(defn dynamic-forms->update-path
  [{:keys [path var-deps sub-forms form]}]
  (if (empty? sub-forms)
    (list path)
    (conj (map dynamic-forms->update-path sub-forms) path)))

(defn dynamic-forms->skips
  [{:keys [path var-deps sub-forms form]}]
  (if (empty? sub-forms)
    [2 0]
    (let [next-skips (map dynamic-forms->skips sub-forms)
          next-firsts (map first next-skips)]
      (into [(+ (apply + next-firsts) 1)]
            (apply concat next-skips)))))

(defn dynamic-forms->preds-and-exprs
  [{:keys [path var-deps sub-forms form]}]
  (if (empty? sub-forms)
    [(var-deps->predicate var-deps) form]
    (into [(var-deps->predicate var-deps)]
          (mapcat dynamic-forms->preds-and-exprs sub-forms))))

(comment
  (binding [*params-changed-sym* {'x 'x123}]
    (let [dyn-forms
          '{:path [2 0],
            :var-deps #{x},
            :form x}]
      [(dynamic-forms->update-path dyn-forms)
       (dynamic-forms->skips dyn-forms)
       (dynamic-forms->preds-and-exprs dyn-forms)]))

  (binding [*params-changed-sym* {'x 'x123 'y 'y234 'z 'z456}]
    (let [dyn-forms
          '{:path [2 0],
            :var-deps #{x y z},
            :sub-forms
            ({:path [0], :var-deps #{x}, :form x}
             {:path [1 0],
              :var-deps #{x y z},
              :sub-forms
              ({:path [0],
                :var-deps #{x y},
                :form (fn* [p1__11076#] (update-in p1__11076# y))}
               {:path [1], :var-deps #{z}, :form z})})}]
      [(dynamic-forms->update-path dyn-forms)
       (dynamic-forms->skips dyn-forms)
       (dynamic-forms->preds-and-exprs dyn-forms)]))
  )

#_(defn dynamic-form->update-expr [{:keys [path var-deps form sub-forms]}]
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

#_(defn dynamic-forms->update-expr [dynamic-forms]
  (if (empty? dynamic-forms) ;; only static
    `identity
    (let [update-exprs (mapcat dynamic-form->update-expr dynamic-forms)]
      `(cond-> ~@update-exprs))))

#_(comment
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
    (vec @extracted)))

(comment
  (extract-params '[{:e r :as rr} y & others])
  )

(defn compile-inc* [content env]
  (let [[static dynamic]
        (binding [*env* env
                  *dynamic-forms* []]
          [(compile-dispatch content []) *dynamic-forms*])]
    (if (empty? dynamic)
      [static]
      (let [dyn-forms (group-dynamic-forms dynamic)
            update-path (dynamic-forms->update-path dyn-forms)
            skips (dynamic-forms->skips dyn-forms)
            preds-and-exprs (dynamic-forms->preds-and-exprs dyn-forms)]
        [static update-path skips preds-and-exprs]))))

;; compile-inc being a macro let us read the cljs analyzer env during macro
;; expansion when compile-inc is used inside a defhtml body
(defmacro compile-inc [content]
  (if *cache-static-counter*
    ;; The html macro is used inside a defhtml
    (let [static-counter *cache-static-counter*]
      (set! *cache-static-counter* (inc *cache-static-counter*))
      (let [[static update-path skips preds-and-exprs]
            (compile-inc* content &env)
            update-path-sym (gensym "update-path")
            skips-sym (gensym "skips")
            statics-sym (gensym "static")]
        (if (nil? update-path) ;; literal content
          `(or (safe-aget ~*cache-sym* "prev-result") '~statics-sym)
          (do
            (set! *update-paths*
                  (into *update-paths* [update-path-sym `'~update-path]))
            (set! *skips* (into *skips* [skips-sym skips]))
            (set! *statics* (into *statics* [statics-sym static]))
            `(ewen.inccup.incremental.compiler/update-with-cache
              '~statics-sym ~*cache-sym* ~static-counter
              '~update-path-sym '~skips-sym
              (cljs.core/array ~@preds-and-exprs))))))
    ;; The html macro is used outside a defhtml, there is no need for
    ;; caching
    content))

(defn compile-inc-with-params [env content params update-fn-sym]
  (let [env (reduce add-local-in-env env params)
        tracked-vars (loop [tracked-vars {}
                            params params]
                       (if-let [param (first params)]
                         (do (assert (contains? (:locals env) param))
                             (recur (assoc tracked-vars param
                                           {:env (get (:locals env) param)
                                            :is-used false
                                            :symbol param})
                                    (rest params)))
                         tracked-vars))]
    (binding [*tracked-vars* tracked-vars
              *cache-sym* (gensym "cache")
              *params-changed-sym* (zipmap params (map gensym params))
              *update-paths* []
              *skips* []
              *statics* []]
      (let [[static update-path skips preds-and-exprs]
            (compile-inc* content env)]
        (if (nil? update-path) ;; literal content
          `[~update-fn-sym (fn [~*cache-sym* ~@params
                                ~@(vals *params-changed-sym*)]
                             (or
                              (ewen.inccup.incremental.compiler/safe-aget
                               ~*cache-sym* "prev-result")
                              ~static))]
          `[~@*update-paths* ~@*skips* ~@*statics*
            skips# ~skips
            update-path# '~update-path
            ~update-fn-sym
            (fn [~*cache-sym* ~@params
                 ~@(vals *params-changed-sym*)]
              (->
               (ewen.inccup.incremental.compiler/safe-aget
                ~*cache-sym* "prev-result")
               (or ~static)
               (ewen.inccup.incremental.compiler/assoc-in-tree
                update-path# skips#
                (cljs.core/array
                 ~@preds-and-exprs))))])))))

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
        `(do
           (set! ewen.inccup.incremental.compiler/*implicit-param*
                 (ewen.inccup.incremental.compiler/get-static-cache
                  ~*cache-sym* ~(dec *cache-static-counter*)))
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
 )
