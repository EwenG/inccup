(ns ewen.inccup.common.compiler
  (:require [clojure.string :as str]
            [ewen.inccup.common.runtime :as runtime]))

(def ^:dynamic *dynamic-forms* nil)

(def ^{:doc "Regular expression that parses a CSS-style id and class
from an element name."
       :private true}
  re-tag #"([^\s\.#]+)(?:#([^\s\.#]+))?(?:\.([^\s#]+))?")

(deftype DynamicLeaf [index])

(defn unevaluated?
  "True if the expression has not been evaluated."
  [expr]
  (or (symbol? expr)
      (and (seq? expr)
           (not= (first expr) `quote))))

(defn literal?
  "True if x is a literal value that can be fully compiled"
  [x]
  (or (and (not (unevaluated? x))
           (not (or (vector? x) (map? x))))
      (and (not (unevaluated? x))
           (every? literal? x))))

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

(defn- form-name
  "Get the name of the supplied form."
  [form]
  (if (and (seq? form) (symbol? (first form)))
    (name (first form))))

(defn- not-implicit-map?
  "True if we can infer that x is not a map."
  [x]
  (or (= (form-name x) "for")
      (not (unevaluated? x))
      (not-hint? x java.util.Map)))

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
                                    (str/replace ^String class "." " "))))
        map-attrs        (first content)]
    (if (map? map-attrs)
      [tag (runtime/merge-attributes tag-attrs map-attrs) (next content)]
      [tag tag-attrs content])))

(defn update-dynamic-forms
  ([expr path type]
   (update-dynamic-forms expr path type nil))
  ([expr path type opts]
   (when *dynamic-forms*
     (set! *dynamic-forms*
           (conj *dynamic-forms*
                 (merge opts
                        {:path path
                         :form expr
                         :index (count *dynamic-forms*)
                         :type type})))
     (-> (count *dynamic-forms*) dec ->DynamicLeaf))))

(defn compile-dynamic-expr [expr path]
  (update-dynamic-forms expr path :child))

(defn compile-attr-map [attrs path]
  (update-dynamic-forms attrs path :attrs))

(defn maybe-attr-map
  ([attrs attr-path path tag-attrs]
   {:pred [(not nil? attrs)
           (not (some c-comp/unevaluated? (mapcat identity attrs)))]}
   [(update-dynamic-forms
     attrs attr-path :maybe-attrs {:tag-attrs tag-attrs})
    (update-dynamic-forms attrs path :maybe-first-child)]))

(defn dynamic-tag [tag path]
  {:pred [(not (c-comp/literal? tag))]}
  (update-dynamic-forms tag path :tag))

(defn element-compile-strategy
  "Returns the compilation strategy to use for a given element."
  [[tag attrs & content :as element] path]
  (cond
    (every? literal? element)
    ::all-literal
    (and (literal? tag)
         (map? attrs)
         (every? literal? attrs))
    ::literal-tag-and-literal-attributes
    (and (literal? tag)
         (map? attrs))
    ::literal-tag-and-map-attributes
    (and (literal? tag) (not-implicit-map? attrs))
    ::literal-tag-and-no-attributes
    (literal? tag)
    ::literal-tag
    (and (map? attrs) (every? literal? attrs))
    ::literal-attributes
    (map? attrs)
    ::map-attributes
    (not-implicit-map? attrs)
    ::no-attributes))

(declare compile-seq)

(defmulti compile-element element-compile-strategy)

(defmethod compile-element ::all-literal
  [element path]
  (let [[tag attrs content] (normalize-element element)]
    (into [tag attrs] content)))

(defmethod compile-element ::literal-tag-and-literal-attributes
  [[tag attrs & content] path]
  (let [[tag attrs _] (normalize-element [tag attrs])]
    (into [tag attrs] (compile-seq content path 2))))

(defmethod compile-element ::literal-tag-and-map-attributes
  [[tag attrs & content] path]
  (let [[tag attrs _] (normalize-element [tag attrs])]
    (into [tag (compile-attr-map attrs (conj path 1))]
          (compile-seq content path 2))))

(defmethod compile-element ::literal-tag-and-no-attributes
  [[tag & content] path]
  (compile-element (apply vector tag {} content) path))

(defmethod compile-element ::literal-tag
  [[tag attrs & content :as element] path]
  (let [[tag tag-attrs [first-content & rest-content]]
        (normalize-element element)
        [attrs-dyn-leaf first-child-dyn-leaf]
        (maybe-attr-map first-content (conj path 1)
                        (conj path 2) tag-attrs)]
    (into [tag attrs-dyn-leaf first-child-dyn-leaf]
          (compile-seq rest-content path 3))))

(defmethod compile-element ::literal-attributes
  [[tag attrs & rest-content] path]
  (into [(dynamic-tag tag (conj path 0)) attrs]
        (compile-seq rest-content path 2)))

(defmethod compile-element ::map-attributes
  [[tag attrs & rest-content] path]
  (into [(dynamic-tag tag (conj path 0))
         (compile-attr-map attrs (conj path 1))]
        (compile-seq rest-content path 2)))

(defmethod compile-element ::no-attributes
  [[tag & content] path]
  (compile-element (apply vector tag {} content) path))

(defmethod compile-element :default
  [[tag attrs & rest-content :as element] path]
  (if (= 1 (count element))
    [(dynamic-tag tag (conj path 0)) {}]
    (let [dyn-tag-leaf (dynamic-tag tag (conj path 0))
          [attrs-dyn-leaf first-child-dyn-leaf]
          (maybe-attr-map attrs (conj path 1) (conj path 2) {})]
      (into [dyn-tag-leaf attrs-dyn-leaf first-child-dyn-leaf]
            (compile-seq rest-content path 3)))))

(declare compile-dispatch)

(defn- compile-seq
  "Compile a sequence of data-structures into HTML."
  ([content path]
   (compile-seq content path 0))
  ([content path index-init]
   (loop [[expr & rest-content :as content] content
          index index-init
          compiled []]
     (if (not (empty? content))
       (let [compiled-expr (compile-dispatch expr (conj path index))]
         (recur rest-content (inc index) (conj compiled compiled-expr)))
       compiled))))

(defn compile-dispatch [expr path]
  (cond
    (vector? expr) (compile-element expr path)
    (string? expr) expr
    (keyword? expr) expr
    (literal? expr) expr
    (seq? expr) (compile-dynamic-expr expr path)
    :else (compile-dynamic-expr expr path)))
