(ns ewen.inccup.common.compiler
  (:require [clojure.string :as str]
            [ewen.inccup.common.runtime :as c-runtime]))

(def ^:dynamic *dynamic-forms* nil)

(def ^{:doc "Regular expression that parses a CSS-style id and class
from an element name."
       :private true}
  re-tag #"([^\s\.#]+)(?:#([^\s\.#]+))?(?:\.([^\s#]+))?")

(deftype DynamicLeaf [index])

(defn coll->js-array [coll]
  (if (coll? coll)
    `(cljs.core/array ~@(map coll->js-array coll))
    coll))

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

(defn merge-attributes [tag-attrs expr]
  (let [id (:id expr)
        merged-attrs (merge-with
                      (fn [attr1 attr2]
                        (cond (and attr1 (string? attr2))
                              (str attr1 " " attr2)
                              attr1
                              `(cljs.core/str ~(str attr1 " ") ~attr2)
                              :else attr2))
                      tag-attrs expr)]
    (if id (assoc merged-attrs :id id) merged-attrs)))

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
                                  (when class
                                    (str/replace ^String class "." " "))))
        map-attrs        (first content)]
    (if (map? map-attrs)
      [tag (merge-attributes tag-attrs map-attrs) (next content)]
      [tag tag-attrs content])))

(defn update-dynamic-forms [expr path original-expr]
  (when *dynamic-forms*
    (set! *dynamic-forms*
          (conj *dynamic-forms*
                {:path path
                 :form expr
                 :original-form original-expr
                 :index (count *dynamic-forms*)}))
    (-> (count *dynamic-forms*) dec ->DynamicLeaf)))

(defn compile-dynamic-expr [expr path]
  (update-dynamic-forms expr path expr))

(defn compile-attr-map [attrs path]
  (update-dynamic-forms attrs path attrs))

(defn maybe-attr-map
  ([attrs attr-path path tag-attrs]
   {:pred [(not nil? attrs)
           (not (some c-comp/unevaluated? (mapcat identity attrs)))]}
   [(update-dynamic-forms
     `(c-runtime/maybe-merge-attributes ~tag-attrs ~attrs) attr-path attrs)
    (update-dynamic-forms `(c-runtime/maybe-first-child) path attrs)]))

(defn dynamic-tag [tag path]
  {:pred [(not (c-comp/literal? tag))]}
  (update-dynamic-forms tag path tag))

(defn element-compile-strategy
  "Returns the compilation strategy to use for a given element."
  [[tag attrs & content :as element] path reverse-order?]
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
  [element path reverse-order?]
  (let [[tag attrs content] (normalize-element element)]
    (into [(name tag) attrs]
          (compile-seq content path reverse-order?))))

(defmethod compile-element ::literal-tag-and-literal-attributes
  [[tag attrs & content] path reverse-order?]
  (let [[tag attrs _] (normalize-element [tag attrs])]
    (into [(name tag) attrs]
          (compile-seq content path reverse-order?))))

(defmethod compile-element ::literal-tag-and-map-attributes
  [[tag attrs & content] path reverse-order?]
  (let [[tag attrs _] (normalize-element [tag attrs])]
    (into [(name tag) (compile-attr-map attrs (conj path 1))]
          (compile-seq content path reverse-order?))))

(defmethod compile-element ::literal-tag-and-no-attributes
  [[tag & content] path reverse-order?]
  (compile-element (apply vector tag {} content) path reverse-order?))

(defmethod compile-element ::literal-tag
  [[tag attrs & content :as element] path reverse-order?]
  (let [[tag tag-attrs [first-content & rest-content]]
        (normalize-element element)
        [attrs-dyn-leaf first-child-dyn-leaf]
        (maybe-attr-map first-content (conj path 1)
                        (conj path 2) tag-attrs)]
    (into [(name tag) attrs-dyn-leaf]
          (compile-seq rest-content path reverse-order?
                       first-child-dyn-leaf))))

(defmethod compile-element ::literal-attributes
  [[tag attrs & rest-content] path reverse-order?]
  (into [(dynamic-tag tag (conj path 0)) attrs]
        (compile-seq rest-content path reverse-order?)))

(defmethod compile-element ::map-attributes
  [[tag attrs & rest-content] path reverse-order?]
  (into [(dynamic-tag tag (conj path 0))
         (compile-attr-map attrs (conj path 1))]
        (compile-seq rest-content path reverse-order?)))

(defmethod compile-element ::no-attributes
  [[tag & content] path reverse-order?]
  (compile-element (apply vector tag {} content) path reverse-order?))

(defmethod compile-element :default
  [[tag attrs & rest-content :as element] path reverse-order?]
  (if (= 1 (count element))
    [(dynamic-tag tag (conj path 0)) {}]
    (let [dyn-tag-leaf (dynamic-tag tag (conj path 0))
          [attrs-dyn-leaf first-child-dyn-leaf]
          (maybe-attr-map attrs (conj path 1) (conj path 2) {})]
      (into [dyn-tag-leaf attrs-dyn-leaf]
            (compile-seq rest-content path reverse-order?
                         first-child-dyn-leaf)))))

(declare compile-dispatch)

(defn- compile-seq* [content path reverse-order? init-seq]
  (loop [[expr & rest-content :as content] content
         index (if (empty? init-seq) 2 3)
         compiled init-seq]
    (if (not (empty? content))
      (let [compiled-expr (compile-dispatch expr (conj path index)
                                            reverse-order?)]
        (recur rest-content (inc index) (conj compiled compiled-expr)))
      compiled)))

(defn- compile-seq
  "Compile a sequence of data-structures into HTML."
  ([content path reverse-order?]
   (compile-seq* content path reverse-order? (if reverse-order? '() [])))
  ([content path reverse-order? first-child]
   (compile-seq* content path reverse-order? (if reverse-order?
                                               (list first-child)
                                               [first-child]))))

(defn compile-dispatch
  ([expr path] (compile-dispatch expr path false))
  ([expr path reverse-order?]
   (cond
     (vector? expr) (compile-element expr path reverse-order?)
     (string? expr) expr
     (keyword? expr) expr
     (literal? expr) expr
     (seq? expr) (compile-dynamic-expr expr path)
     :else (compile-dynamic-expr expr path))))

#_(defn handle-void-tags [x]
  (if (vector? x)
    (let [[tag attrs & children] x
          m (meta x)]
      (cond-> x
        (get c-runtime/void-tags tag) (subvec 0 2)
        m (with-meta m)))
    x))

(defn walk-static* [inner outer f form]
  (let [transformed (inner form)]
    (if (vector? transformed)
      (let [[tag attrs & children] transformed
            m (meta transformed)]
        (cond-> (subvec transformed 0 2)
          true (into (doall (map f children)))
          m (with-meta m)
          true outer))
      (outer transformed))))

(defn walk-static [inner outer static]
  (walk-static* inner outer (partial walk-static inner outer) static))

(comment
  (walk-static
   identity identity
   ["div" {:id "ii", :class "cc"} ["p" {} ["p2" {}]]
    (->DynamicLeaf 1)])
  )
