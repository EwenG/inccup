(ns ewen.inccup.compiler-data-macros
  (:require [ewen.inccup.compiler-data :as comp]
            [ewen.inccup.emitter :as emitter
             :refer [*tracked-vars* track-vars]]
            [cljs.analyzer.api :refer [analyze empty-env]]
            [clojure.walk :refer [postwalk]]
            [clojure.set :refer [union]]))

(def ^:dynamic *cache-sym* nil)
(def ^:dynamic *cache-counter* nil)
(def ^:dynamic *params-changed-sym* nil)
(def ^:dynamic *dynamic-forms* nil)

(defn dynamic-result [expr path]
  (when *dynamic-forms*
    (binding [*tracked-vars* *tracked-vars*]
      (let [cljs-expanded (track-vars expr)
            used-vars (keep #(when (:is-used %) (:symbol %))
                            (vals *tracked-vars*))]
        (if (empty? used-vars)
          cljs-expanded
          (do (set! *dynamic-forms*
                    (conj *dynamic-forms*
                          {:path path
                           :var-deps (set used-vars)
                           :form cljs-expanded}))
              nil))))))

(defn- unevaluated?
  "True if the expression has not been evaluated."
  [expr]
  (or (symbol? expr)
      (and (seq? expr)
           (not= (first expr) `quote))))

(defn compile-attr-map
  "Returns an unevaluated form that will render the supplied map as HTML
  attributes."
  [attrs path]
  (if (some unevaluated? (mapcat identity attrs))
    (dynamic-result attrs path) attrs))

(defn- form-name
  "Get the name of the supplied form."
  [form path]
  (if (and (seq? form) (symbol? (first form)))
    (name (first form))))

(declare compile-html)

(defmulti compile-form
  "Pre-compile certain standard forms, where possible."
  {:private true}
  form-name)

(defmethod compile-form :default
  [expr path] (dynamic-result expr path))

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
  (and (not (unevaluated? x))
       (or (not (or (vector? x) (map? x)))
           (every? literal? x))))

(defn- not-implicit-map?
  "True if we can infer that x is not a map."
  [x]
  (or (= (form-name x) "for")
      (not (unevaluated? x))
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
  (let [compiled-attrs (compile-attr-map attrs (conj path 1))]
    (if (#'comp/container-tag? tag content)
      (into [tag compiled-attrs] (compile-seq content path 2))
      [tag compiled-attrs])))

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
    (hint? expr String) expr
    (hint? expr Number) expr
    (seq? expr) (compile-form expr path)
    :else (dynamic-result expr path)))

(defn compute-common-path [dynamic-forms]
  (let [var-deps (apply union (map :var-deps dynamic-forms))
        path-groups (->> (map :path dynamic-forms)
                         (apply interleave)
                         (partition (count dynamic-forms)))]
    [(loop [[path-group & rest-path-groups] path-groups
            common-path []]
       (if (or (nil? path-group) (not (apply = path-group)))
         common-path
         (recur rest-path-groups (conj common-path (first path-group)))))
     var-deps]))

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
  (compute-common-path [{:path [2 0 0] :var-deps #{1}}
                {:path [2 0 1 0 0] :var-deps #{1 2}}
                        {:path [2 0 1 0 1]}])

  (group-dynamic-forms '[{:path [0 1] :var-deps #{x} :form x}
                         {:path [2 0 0] :var-deps #{x} :form x}
                         {:path [2 0 1 0 0] :var-deps #{x y} :form y}
                         {:path [2 0 1 0 1] :var-deps #{z} :form z}])

  ;; Should throw an error
  (group-dynamic-forms '[{:path [0 1] :var-deps #{x} :form x}
                         {:path [2 0 1] :var-deps #{x} :form x}
                         {:path [2 0 1 0 0] :var-deps #{x y} :form y}
                         {:path [2 0 1 0 1] :var-deps #{z} :form z}])
  )

(declare dynamic-forms->update-expr)

(defn var-deps->predicate [var-deps]
  (cond (empty? var-deps) false
        (nil? *params-changed-sym*) true
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
      (update-in ~path (constantly ~form)))
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

(defn compile-data* [content env]
  (let [[static dynamic]
        (binding [emitter/*env* env
                  *dynamic-forms* []
                  *cache-counter* (when *cache-counter*
                                    (inc *cache-counter*))]
          [(compile-dispatch content []) *dynamic-forms*])
        update-expr (dynamic-forms->update-expr dynamic)
        cached-sym (gensym "cached")]
    `(let [~cached-sym (or (get @~*cache-sym* ~*cache-counter*) ~static)
           ~cached-sym (~update-expr ~cached-sym)]
       ~(when *cache-counter*
          `(when ~*cache-sym*
             (swap! ~*cache-sym*
                    assoc ~*cache-counter* ~cached-sym)))
       ~cached-sym)))

(defn with-params-changed [params params-changed-sym cache-sym & body]
  (let [changed-bindings
        (mapcat (fn [param]
                  [(get params-changed-sym param)
                   `(or (not (:params @~cache-sym))
                        (not= ~param (get (:params @~cache-sym) '~param)))])
                params)]
    `(let ~(vec changed-bindings) ~@body)))

(defmacro compile-data
  ([content] (compile-data* content &env))
  ([content params cache-sym]
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
               *cache-counter* 0
               *cache-sym* cache-sym]
       (with-params-changed params params-changed-sym cache-sym
         `(swap! ~*cache-sym* assoc :params
                 ~(conj params-with-sym `hash-map))
         (compile-data* content &env))))))

(comment
  (require '[clojure.pprint :refer [pprint pp]])

  (binding [*dynamic-forms* #{}]
    (compile-element '[:e {} [:p {} x]] []))

  (binding [*cache-counter* 0]
    (let [x "c"]
      (compile-data '[:e {} [:p {} x]])))
 )
