(ns ewen.inccup.compiler-data-macros
  (:require [ewen.inccup.compiler-data :as comp]))

(def ^:dynamic *cache-counter* nil)

(defn- unevaluated?
  "True if the expression has not been evaluated."
  [expr]
  (or (symbol? expr)
      (and (seq? expr)
           (not= (first expr) `quote))))

(defn compile-attr-map
  "Returns an unevaluated form that will render the supplied map as HTML
  attributes."
  [attrs tracked-vars]
  (if (some unevaluated? (mapcat identity attrs))
    {:static nil
     :dynamic {:tx attrs}}
    {:static attrs}))

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
  [[tag attrs & content :as element] tracked-vars]
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
  [element tracked-vars]
  {:static element})

(defmethod compile-element ::literal-tag-and-attributes
  [[tag attrs & content] tracked-vars]
  (let [compiled-attrs (compile-attr-map
                        attrs tracked-vars)
        attrs-tx (get-in compiled-attrs [:dynamic :tx])]
    (if (#'comp/container-tag? tag content)
      (let [compiled-content (compile-seq content  tracked-vars)]
        {:static [tag (:static compiled-attrs)
                  ;; Retun a vector instead of a seq because it must be
                  ;; compatible with update-in
                  (mapv :static compiled-content)]
         :dynamic {:nested-tx (list {:path 2
                                     :nested-tx (map :dynamic
                                                     compiled-content)}
                                    {:path 1 :tx attrs-tx})}})
      {:static [tag (:static compiled-attrs)]
       :dynamic {:nested-tx {:path 1 :tx attrs-tx}}})))

(defn- compile-seq
  "Compile a sequence of data-structures into HTML."
  [content tracked-vars]
  (loop [[expr & rest-content] content
         index 0
         compiled []]
    (if expr
      (let [compiled-expr
            (cond
              (vector? expr) (assoc-in
                              (compile-element expr tracked-vars)
                              [:dynamic :path] index)
              :else {:static nil
                     :dynamic {:path index :tx expr}})]
        (recur rest-content (inc index) (conj compiled compiled-expr)))
      ;; Retun a vector instead of a seq because it must be
      ;; compatible with update-in
      compiled)))

(defn group-nested-paths [grouped-path
                          {:keys [path tx nested-tx] :as dyn-element}]
  (let [updated-path (conj grouped-path path)]
    (if (or tx (not (= 1 (count nested-tx))))
      [updated-path dyn-element]
      (recur updated-path (first nested-tx)))))

(defn dynamic->update-expr [{:keys [tx nested-tx]}]
  (let [update-exprs (map (partial group-nested-paths []) nested-tx)
        update-exprs (filter (fn [[path {:keys [tx nested-tx]}]]
                               (or tx (not (empty? nested-tx))))
                             update-exprs)
        update-exprs (map (fn [[path dyn-element]]
                            `(update-in
                              ~path ~(dynamic->update-expr dyn-element)))
                          update-exprs)]
    (if tx
      `(constantly ~tx)
      `(fn [form#]
         (-> form# ~@update-exprs)))))

(comment
  (dynamic->update-expr '{:nested-tx
                          ({:path 2, :nested-tx ({:path 0, :tx x})}
                           {:path 1, :tx nil})})
  )

(defn compile-data [content]
  (let [{:keys [static dynamic]} (compile-element content #{})
        cached-sym (gensym "cached")
        update-expr (when dynamic (dynamic->update-expr dynamic))
        ret `(let [~cached-sym (if comp/*cache*
                                 (get comp/*cache* ~*cache-counter*)
                                 ~static)
                   ~@(when update-expr
                       [cached-sym `(~update-expr ~cached-sym)])]
               ~(when *cache-counter*
                  `(when comp/*cache*
                     (set! comp/*cache*
                           (assoc
                            comp/*cache* ~*cache-counter* ~cached-sym))))
           ~cached-sym)]
    (when *cache-counter* (set! *cache-counter* (inc *cache-counter*)))
    ret))

(comment
  (require '[clojure.pprint :refer [pprint pp]])

  (compile-element '[:e {} x] #{})

  (binding [*cache-counter* 0]
    (let [x "c"]
      (compile-data '[:e {} x])))
 )
