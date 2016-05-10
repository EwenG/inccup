(ns ewen.inccup.incremental.compiler-macros
  (:require [ewen.inccup.incremental.emitter :as emitter]
            [ewen.inccup.util :as util]
            [cljs.analyzer.api :as ana-api]
            [clojure.walk :refer [postwalk]]
            [clojure.set :refer [union]]
            [cljs.tagged-literals :refer [*cljs-data-readers*]]))

(def ^:dynamic *dynamic-forms* nil)
(def ^:dynamic *tracked-vars* #{})
(def ^:dynamic *env* nil)
(def ^:dynamic *update-paths* [])
(def ^:dynamic *skips* [])
(def ^:dynamic *statics* [])
(defonce component-id (atom 0))

(defn new-local-binding [name env]
  {:name name
   :binding-form? true
   :op :var
   :env env
   :info {:name name, :shadow nil}
   :shadow nil
   :local true})

(defn track-vars [expr]
  (binding [*tracked-vars* *tracked-vars*]
    (let [env (or *env* (ana-api/empty-env))
          cljs-expanded (-> env (ana-api/analyze expr) emitter/emit*)
          used-vars (keep #(when (:is-used %) (:symbol %))
                          (vals *tracked-vars*))]
      [cljs-expanded (set used-vars)])))

(defn update-dynamic-forms [expr path used-vars form-type]
  (when *dynamic-forms*
    (set! *dynamic-forms*
          (conj *dynamic-forms*
                {:path path
                 :var-deps used-vars
                 :form expr
                 :type form-type}))))

(defn compile-dynamic-expr [expr path]
  (let [[expr used-vars] (track-vars expr)]
    (update-dynamic-forms `(cljs.core/constantly ~expr)
                          path used-vars "child")
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
           `#(ewen.inccup.incremental.compiler/maybe-merge-attributes
              ~tag-attrs ~expr)
           form `#(if (map? ewen.inccup.incremental.compiler/*tmp-val*)
                    nil
                    ewen.inccup.incremental.compiler/*tmp-val*)]
       (update-dynamic-forms attr-form attr-path used-vars "maybe-attrs")
       (update-dynamic-forms form path used-vars "or-attrs-child")
       nil))))

(defn- literal?
  "True if x is a literal value that can be rendered as-is."
  [x]
  (and (not (util/unevaluated? x))
       (or (not (or (vector? x) (map? x)))
           (every? literal? x))))

(defn dynamic-tag [tag path]
  {:pred [(not (literal? tag))]}
  (let [[expr used-vars] (track-vars tag)]
    (update-dynamic-forms `(cljs.core/constantly ~expr)
                          path used-vars "tag")
    nil))

(defn- form-name
  "Get the name of the supplied form."
  [form]
  (if (and (seq? form) (symbol? (first form)))
    (name (first form))))

(declare compile-html)

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

(defn literal->js [x]
  (cond
    (nil? x) nil
    (string? x) x
    (number? x) (str x)
    (keyword? x) (name x)
    (symbol? x) (str x)
    (vector? x) (if (nil? (first x))
                  (let [[tag attrs & content] x]
                    `(cljs.core/array ~tag
                                      ~(literal->js attrs)
                                      ~@(map literal->js content)))
                  (let [[tag attrs content] (util/normalize-element x)]
                    `(cljs.core/array ~tag
                                      ~(literal->js attrs)
                                      ~@(map literal->js content))))
    (map? x) `(cljs.core/js-obj
               ~@(interleave (map name (keys x))
                             (map literal->js (vals x))))
    :else (throw (IllegalArgumentException.
                  (str "Not a literal element: " x)))))

(comment
  (literal->js '[:e.c {:class "c2"} "e"])
  )

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
  (let [[tag attrs _] (util/normalize-element [tag attrs])
        compiled-attrs (compile-attr-map attrs (conj path 1))]
    (into [tag (or compiled-attrs {})] (compile-seq content path 2))))

(defmethod compile-element ::literal-tag-and-no-attributes
  [[tag & content] path]
  (compile-element (apply vector tag {} content) path))

(defmethod compile-element ::literal-tag
  [[tag attrs & content :as element] path]
  (let [[tag tag-attrs [first-content & rest-content]]
        (util/normalize-element element)]
    (maybe-attr-map first-content (conj path 1) (conj path 2) tag-attrs)
    (into [tag (or tag-attrs {}) nil] (compile-seq rest-content path 3))))

(defmethod compile-element ::default
  [[tag attrs & rest-content :as element] path]
  (dynamic-tag tag (conj path 0))
  (maybe-attr-map attrs (conj path 1) (conj path 2) {})
  (if (= 1 (count element))
    [nil {}]
    (into [nil {} nil] (compile-seq rest-content path 3))))

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
    (seq? expr) (compile-dynamic-expr expr path)
    :else (compile-dynamic-expr expr path)))

(defn var-deps->indexes [params var-deps]
  (map #(.indexOf params %) var-deps))

(defmacro component [forms]
  (let [params (->> (:locals &env)
                    (filter (comp not :local second))
                    (filter (comp not :fn-var second)))
        tracked-vars (loop [tracked-vars {}
                            params params]
                       (if-let [[param-name param-env] (first params)]
                         (recur (assoc tracked-vars param-name
                                       {:env param-env
                                        :is-used false
                                        :symbol param-name})
                                (rest params))
                         tracked-vars))
        [static dynamic]
        (binding [*env* &env
                  *dynamic-forms* []
                  *tracked-vars* tracked-vars]
          [(compile-dispatch forms []) *dynamic-forms*])
        static (literal->js static)
        var-deps->indexes (partial var-deps->indexes (keys tracked-vars))
        coll->cljs-array (fn [coll] `(cljs.core/array ~@coll))]
    `(ewen.inccup.incremental.compiler/Component.
      (cljs.core/constantly ~static)
      (cljs.core/array ~@(map (comp coll->cljs-array :path) dynamic))
      (cljs.core/array ~@(->> dynamic
                              (map :var-deps)
                              (map var-deps->indexes)
                              (map coll->cljs-array)))
      (cljs.core/array ~@(map :type dynamic))
      (cljs.core/array ~@(map :form dynamic))
      (cljs.core/array ~@(keys tracked-vars))
      ~(swap! component-id inc) nil)))

(alter-var-root #'*cljs-data-readers* assoc 'h
                (fn [forms]
                  {:pre [(vector? forms)]}
                  `(component ~forms)))

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
