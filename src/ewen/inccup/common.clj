(ns ewen.inccup.common
  (:require [clojure.string :as str]))

(def ^{:doc "Regular expression that parses a CSS-style id and class
from an element name."
       :private true}
  re-tag #"([^\s\.#]+)(?:#([^\s\.#]+))?(?:\.([^\s#]+))?")

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

(defn element-compile-strategy
  "Returns the compilation strategy to use for a given element."
  ([params]
   (element-compile-strategy params nil))
  ([[tag attrs & content :as element] path]
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
     ::no-attributes
     :else
     ::default)))

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
      [tag (merge-attributes tag-attrs map-attrs) (next content)]
      [tag tag-attrs content])))
