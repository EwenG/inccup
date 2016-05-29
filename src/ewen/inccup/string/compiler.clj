(ns ewen.inccup.string.compiler
  "Internal functions for compilation."
  (:require [ewen.inccup.util :as util]
            [ewen.inccup.common :as common :refer [literal?]]
            [ewen.inccup.string.runtime :as runtime])
  (:import [clojure.lang IPersistentVector ISeq Named]))

#_(declare compile-seq)

#_(defmulti compile-element
  {:private true}
  common/element-compile-strategy)

#_(defmethod compile-element ::all-literal
  [element]
  (#'runtime/render-element (eval element)))

#_(defmethod compile-element ::literal-tag-and-attributes
  [[tag attrs & content]]
  (let [[tag attrs _] (runtime/normalize-element [tag attrs])]
    (if (#'runtime/container-tag? tag content)
      `(str ~(str "<" tag) ~(compile-attr-map attrs) ">"
            ~@(compile-seq content)
            ~(str "</" tag ">"))
      `(str "<" ~tag ~(compile-attr-map attrs) ~(#'runtime/end-tag)))))

#_(defmethod compile-element ::literal-tag-and-no-attributes
  [[tag & content]]
  (compile-element (apply vector tag {} content)))

#_(defmethod compile-element ::literal-tag
  [[tag attrs & content]]
  (let [[tag tag-attrs _] (runtime/normalize-element [tag])
        attrs-sym         (gensym "attrs")]
    `(let [~attrs-sym ~attrs]
       (if (map? ~attrs-sym)
         ~(if (#'runtime/container-tag? tag content)
            `(str ~(str "<" tag)
                  (runtime/render-attr-map
                   (merge ~tag-attrs ~attrs-sym)) ">"
                  ~@(compile-seq content)
                  ~(str "</" tag ">"))
            `(str ~(str "<" tag)
                  (runtime/render-attr-map (merge ~tag-attrs ~attrs-sym))
                  ~(#'runtime/end-tag)))
         ~(if (#'runtime/container-tag? tag attrs)
            `(str ~(str "<" tag (runtime/render-attr-map tag-attrs) ">")
                  ~@(compile-seq (cons attrs-sym content))
                  ~(str "</" tag ">"))
            (str "<" tag (runtime/render-attr-map tag-attrs)
                 (#'runtime/end-tag)))))))

#_(defmethod compile-element :default
  [element]
  `(runtime/render-element
     [~(first element)
      ~@(for [x (rest element)]
          (if (vector? x)
            (compile-element x)
            x))]))

#_(defn- compile-seq
  "Compile a sequence of data-structures into HTML."
  [content]
  (doall (for [expr content]
           (cond
             (vector? expr) (compile-element expr)
             (string? expr) (escape-html expr)
             (keyword? expr) (escape-html (name expr))
             (raw-string? expr) expr
             (literal? expr) (escape-html expr)
             (hint? expr String) `(escape-html ~expr)
             (hint? expr Number) expr
             (seq? expr) (compile-form expr)
             :else `(runtime/render-html ~expr)))))

#_(defn- collapse-strs
  "Collapse nested str expressions into one, where possible."
  [expr]
  (if (seq? expr)
    (cons
     (first expr)
     (mapcat
      #(if (and (seq? %) (symbol? (first %))
                (= (first %) (first expr) `str))
         (rest (collapse-strs %))
         (list (collapse-strs %)))
      (rest expr)))
    expr))

#_(defn compile-html
  "Pre-compile data structures into HTML where possible."
  [content]
  ;;Wrap the result in a RawString object in order to be able to
  ;;differentiate compiled strings from other strings.
  (collapse-strs `(raw-string ~@(compile-seq (list content)))))

#_(defn maybe-convert-raw-string [compile-fn content]
  `(let [out-str# (binding [*is-top-level* false]
                    ~(compile-fn content))]
     (if *is-top-level*
       (str out-str#) out-str#)))













(def ^{:doc "A list of elements that must be rendered without a closing tag."
       :private true}
  void-tags
  #{"area" "base" "br" "col" "command" "embed" "hr" "img" "input"
    "keygen" "link" "meta" "param" "source" "track" "wbr"})

(defn- xml-mode? []
  (get #{:xml :xhtml} util/*html-mode*))

(defn- html-mode? []
  (get #{:html :xhtml} util/*html-mode*))

(defn- end-tag []
  (if (xml-mode?) " />" ">"))

(defn- container-tag?
  "Returns true if the tag has content or is not a void tag.
  In non-HTML modes, all contentless tags are assumed to be void tags."
  [tag content]
  (or (not (empty? content))
      (and (html-mode?) (not (void-tags tag)))))

(defn- xml-attribute [k v]
  (str " " (name k) "=\"" (util/escape-string v) "\""))

(defn- compile-attr [[k v]]
  (cond
    (true? v)
    (if (xml-mode?)
      (xml-attribute k k)
      (str " " (name k)))
    (not v)
    ""
    :else
    (xml-attribute k v)))

(defn- compile-attrs
  [attrs]
  (apply str (map compile-attr attrs)))

(deftype RawString [^String s]
  Object
  (^String toString [this] s)
  (^boolean equals [this other]
   (and (instance? RawString other)
        (= s  (.toString other)))))

(defn raw-string
  "Wraps a string to an object that will be pasted to HTML without escaping."
  ([] (RawString. ""))
  ([x] (RawString. x))
  ([x & xs] (RawString. (apply str x xs))))

(defn raw-string?
  "Returns true if x is a RawString"
  [x]
  (instance? RawString x))

(defprotocol StringRenderer
  (element->string [this]))

(defn form->string
  [[tag attrs & content]]
  (if (container-tag? tag content)
    (str "<" tag (compile-attrs attrs) ">"
         (element->string content)
         "</" tag ">")
    (str "<" tag (compile-attrs attrs) (end-tag))))

(extend-protocol StringRenderer
  IPersistentVector
  (element->string [this]
    (form->string this))
  ISeq
  (element->string [this]
    (apply str (map element->string this)))
  RawString
  (element->string [this]
    (str this))
  Named
  (element->string [this]
    (util/escape-string (name this)))
  Object
  (element->string [this]
    (util/escape-string (str this)))
  nil
  (element->string [this]
    ""))

(declare compile-seq)
(declare compile-dispatch)

(defmulti compile-element
  {:private true}
  common/element-compile-strategy)

(defmethod compile-element ::common/all-literal
  [element]
  (let [[tag attrs content] (common/normalize-element element)]
    (if (container-tag? tag content)
      `(str ~(str "<" tag) ~(compile-attrs attrs) ">"
            ~@(compile-seq content)
            ~(str "</" tag ">"))
      `(str "<" ~tag ~(compile-attrs attrs) ~(end-tag)))))

(defmethod compile-element ::common/literal-tag-and-literal-attributes
  [element]
  (let [[tag attrs content] (common/normalize-element element)]
    (if (container-tag? tag content)
      `(str ~(str "<" tag) ~(compile-attrs attrs) ">"
            ~@(compile-seq content)
            ~(str "</" tag ">"))
      `(str "<" ~tag ~(compile-attrs attrs) ~(end-tag)))))

(defn- compile-seq [content]
  (loop [content content
         compiled []]
    (if-let [expr (first content)]
      (let [compiled-expr (compile-dispatch expr)]
        (recur (rest content) (conj compiled compiled-expr)))
      compiled)))

(defn- compile-dispatch [expr]
  (cond
    (vector? expr) (compile-element expr)
    (string? expr) expr
    (keyword? expr) expr
    (literal? expr) expr
    (seq? expr) `(ewen.inccup.string.runtime/form->string ~expr)
    :else `(ewen.inccup.string.runtime/form->string ~expr)))

(defmacro compile-string [forms]
  (compile-dispatch forms))
