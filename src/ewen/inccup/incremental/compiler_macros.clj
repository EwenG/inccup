(ns ewen.inccup.incremental.compiler-macros
  (:require [ewen.inccup.incremental.compiler :as comp]
            [ewen.inccup.incremental.emitter :as emitter
             :refer [*tracked-vars* track-vars]]
            [cljs.analyzer.api :refer [analyze empty-env]]
            [clojure.walk :refer [postwalk]]
            [clojure.set :refer [union]]))

(def ^:dynamic *cache-sym* nil)
(def ^:dynamic *cache-static-counter* nil)
(def ^:dynamic *params-changed-sym* nil)
(def ^:dynamic *dynamic-forms* nil)

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
                     :form (comp expr prev-form)}))
        (set! *dynamic-forms*
              (conj *dynamic-forms*
                    {:path path
                     :var-deps used-vars
                     :form expr}))))))

(defn compile-dynamic-expr [expr path]
  (let [[expr used-vars] (track-vars expr)]
    (update-dynamic-forms `(constantly ~expr) path used-vars)
    (if (empty? used-vars) expr nil)))

(def ^{:doc "Regular expression that parses a CSS-style id and class from
an element name."
       :private true}
  re-tag #"([^\s\.#]+)(?:#([^\s\.#]+))?(?:\.([^\s#]+))?")

(def ^{:doc "A list of elements that must be rendered without a closing
tag."
       :private true}
  void-tags
  #{"area" "base" "br" "col" "command" "embed" "hr" "img" "input" "keygen"
    "link" "meta" "param" "source" "track" "wbr"})

(defn- container-tag?
  "Returns true if the tag has content or is not a void tag."
  [tag content]
  (or content (not (void-tags tag))))

(defn normalize-element
  "Ensure an element vector is of the form [tag-name attrs content]."
  [[tag & content]]
  (when (not (or (keyword? tag) (symbol? tag) (string? tag)))
    (throw (IllegalArgumentException.
            (str tag " is not a valid element name."))))
  (let [[_ tag id class] (re-matches re-tag (name tag))
        tag-attrs        (cond-> {}
                           id (assoc :id id)
                           class (assoc
                                  :class
                                  (if class
                                    (.replace ^String class "." " "))))
        map-attrs        (first content)]
    (if (map? map-attrs)
      [tag (comp/merge-attributes tag-attrs map-attrs) (next content)]
      [tag tag-attrs content])))

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
       (when expr (update-dynamic-forms form path used-vars ))
       nil))))

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

(defn- literal?
  "True if x is a literal value that can be rendered as-is."
  [x]
  (and (not (comp/unevaluated? x))
       (or (not (or (vector? x) (map? x)))
           (every? literal? x))))

(defn- not-implicit-map?
  "True if we can infer that x is not a map."
  [x]
  (or (= (form-name x) "for")
      (not (comp/unevaluated? x))
      (not-hint? x java.util.Map)))

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
  [element path] element)

(defmethod compile-element ::literal-tag-and-attributes
  [[tag attrs & content] path]
  (let [[tag attrs _] (normalize-element [tag attrs])
        compiled-attrs (compile-attr-map attrs (conj path 1))
        compiled-attrs (if (map? compiled-attrs)
                         (vary-meta
                          compiled-attrs
                          assoc ::comp/shortcut-attrs compiled-attrs)
                         compiled-attrs)]
    (if (container-tag? tag content)
      (into [tag compiled-attrs] (compile-seq content path 2))
      [tag compiled-attrs])))

(defmethod compile-element ::literal-tag-and-no-attributes
  [[tag & content] path]
  (compile-element (apply vector tag {} content) path))

(defmethod compile-element ::literal-tag
  [[tag attrs & content :as element] path]
  (let [[tag tag-attrs [first-content & rest-content]]
        (normalize-element element)
        tag-attrs (if (map? tag-attrs)
                    (vary-meta
                     tag-attrs assoc ::comp/shortcut-attrs tag-attrs)
                    tag-attrs)]
    (if (container-tag? tag content)
      (do
        (maybe-attr-map first-content (conj path 1) (conj path 2))
        (into [tag tag-attrs nil] (compile-seq rest-content path 3)))
      (do
        (maybe-attr-map first-content (conj path 1))
        (into [tag tag-attrs])))))

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
        (empty? var-deps) false
        :else (let [preds (doall
                           (map #(get *params-changed-sym* %) var-deps))]
                (if (= 1 (count preds))
                  (first preds)
                  `(or ~@preds)))))

(defn dynamic-form->update-expr [{:keys [path var-deps form sub-forms]}]
  {:pre [(or (and form (nil? sub-forms))
             (and (nil? form) sub-forms))
         (not (empty? path))]}
  (if form
    `(~(var-deps->predicate var-deps)
      (update-in ~path ~form))
    `(~(var-deps->predicate var-deps)
      (update-in ~path ~(dynamic-forms->update-expr sub-forms)))))

(defn dynamic-forms->update-expr [dynamic-forms]
  (cond (empty? dynamic-forms) ;; only static
        `identity
        (= [] (:path (first dynamic-forms))) ;; only dynamic
        (do
          (assert (= 1 (count dynamic-forms)))
          `(constantly ~(:form (first dynamic-forms))))
        :else
        (let [update-exprs (mapcat dynamic-form->update-expr dynamic-forms)]
          `(fn [form#]
             (cond-> form# ~@update-exprs)))))

(comment
  (binding [*params-changed-sym* {'x 'x123 'y 'y234 'z 'z456}]
    (dynamic-forms->update-expr
     '({:path [0 1] :var-deps #{x} :form x}
       {:path [2 0]
        :var-deps #{x y z}
        :sub-forms
        ({:path [0] :var-deps #{x} :form x}
         {:path [1 0]
          :var-deps #{x y z}
          :sub-forms
          ({:path [0] :var-deps #{x y} :form y}
           {:path [1] :var-deps #{z} :form z})})})))
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
        (binding [emitter/*env* env
                  *dynamic-forms* []]
          [(compile-dispatch content []) *dynamic-forms*])
        update-expr (dynamic-forms->update-expr dynamic)]
    [static update-expr]))

(defn with-params-changed [params params-changed-sym & body]
  (let [changed-bindings
        (mapcat (fn [param]
                  [(get params-changed-sym param)
                   `(or (not (:params comp/*cache*))
                        (not= ~param (get (:params comp/*cache*)
                                          '~param)))])
                params)]
    `(let ~(vec changed-bindings) ~@body)))

(defmacro compile-inc
  ([content]
   (when *cache-static-counter*
     (set! *cache-static-counter* (inc *cache-static-counter*)))
   (let [[static update-expr] (compile-inc* content &env)]
     `(binding [comp/*cache* (comp/get-static-cache
                              comp/*cache* ~(dec *cache-static-counter*))]
        (let [result# (-> (comp/safe-aget comp/*cache* "prev-result")
                          (or ~static)
                          (~update-expr))]
          (comp/safe-aset comp/*cache* "prev-result" result#)
          result#))))
  ([content params]
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
               *cache-static-counter* 0]
       (let [[static update-expr] (compile-inc* content &env)]
         `(binding [comp/*cache* (comp/new-dynamic-cache comp/*cache*)]
            ~(with-params-changed params params-changed-sym
               `(comp/safe-aset
                 comp/*cache* "params" ~(conj params-with-sym `hash-map))
               `(comp/make-static-cache
                 comp/*cache* ~*cache-static-counter*)
               `(let [result# (-> (comp/safe-aget
                                   comp/*cache* "prev-result")
                                  (or ~static)
                                  (~update-expr))]
                  (comp/safe-aset comp/*cache* "prev-result" result#)
                  (comp/clean-sub-cache comp/*cache*)
                  result#))))))))

(comment
  (require '[clojure.pprint :refer [pprint pp]])

  (binding [*dynamic-forms* #{}]
    (compile-element '[:e {} [:p {} x]] []))

  (binding [*cache-static-counter* 0]
    (let [x "c"]
      (compile-inc '[:e {} [:p {} x]])))
 )
