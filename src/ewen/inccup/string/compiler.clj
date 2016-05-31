(ns ewen.inccup.string.compiler
  "Internal functions for compilation."
  (:require [ewen.inccup.common.util :as util]
            [ewen.inccup.common.runtime :as c-runtime]
            [ewen.inccup.common.compiler :as c-comp]
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

(defn compile-attrs [attrs]
  (if (some c-comp/unevaluated? (mapcat identity attrs))
    `(runtime/render-attrs ~attrs)
    (runtime/render-attrs attrs)))

(declare compile-seq)
(declare compile-dispatch)

(defmulti compile-element
  {:private true}
  c-comp/element-compile-strategy)

(defmethod compile-element ::c-comp/all-literal
  [element]
  (let [[tag attrs content] (c-comp/normalize-element element)]
    (if (get c-runtime/void-tags tag)
      `(str "<" ~tag ~(compile-attrs attrs) " />")
      `(str ~(str "<" tag) ~(compile-attrs attrs) ">"
            ~@(compile-seq content)
            ~(str "</" tag ">")))))

(comment
  (require '[ewen.inccup.compiler :refer [h]])
  (h [:div])
  )

(defmethod compile-element ::c-comp/literal-tag-and-literal-attributes
  [element]
  (let [[tag attrs content] (c-comp/normalize-element element)]
    (if (get c-runtime/void-tags tag)
      `(str "<" ~tag ~(compile-attrs attrs) " />")
      `(str ~(str "<" tag) ~(compile-attrs attrs) ">"
            ~@(compile-seq content)
            ~(str "</" tag ">")))))

(comment
  (require '[ewen.inccup.compiler :refer [h]])
  (h [:div {:e "e"}])
  )

(defmethod compile-element ::c-comp/literal-tag-and-map-attributes
  [[tag attrs & content]]
  (let [[tag attrs _] (c-comp/normalize-element [tag attrs])]
    (if (get c-runtime/void-tags tag)
      `(str "<" ~tag ~(compile-attrs attrs) " />")
      `(str ~(str "<" tag) ~(compile-attrs attrs) ">"
            ~@(compile-seq content)
            ~(str "</" tag ">")))))

(comment
  (require '[ewen.inccup.compiler :refer [h]])
  (let [e "ee"] (h [:div {:e e}]))
  )

(defmethod compile-element ::c-comp/literal-tag-and-no-attributes
  [[tag & content]]
  (compile-element (apply vector tag {} content)))

(comment
  (require '[ewen.inccup.compiler :refer [h]])
  (let [e "ee"] (h [:div ^String e]))
  )

(defmethod compile-element ::c-comp/literal-tag
  [[tag attrs & content :as element]]
  (let [[tag tag-attrs [first-content & rest-content :as content]]
        (c-comp/normalize-element element)
        attrs-sym (gensym "attrs")]
    `(let [~attrs-sym ~first-content]
       (if (map? ~attrs-sym)
         ~(if (get c-runtime/void-tags tag)
            `(str ~(str "<" tag)
                  (runtime/render-attrs (c-runtime/merge-attributes
                                         ~tag-attrs ~attrs-sym)) " />")
            `(str ~(str "<" tag)
                  (runtime/render-attrs (c-runtime/merge-attributes
                                         ~tag-attrs ~attrs-sym)) ">"
                  ~@(compile-seq rest-content)
                  ~(str "</" tag ">")))
         ~(if (get c-runtime/void-tags tag)
            (str "<" tag (runtime/render-attrs tag-attrs) " />")
            `(str ~(str "<" tag (runtime/render-attrs tag-attrs) ">")
                  ~@(compile-seq content)
                  ~(str "</" tag ">")))))))

(comment
  (require '[ewen.inccup.compiler :refer [h]])
  (let [e "ee"] (h [:div e]))
  (let [e {:e "e"}] (h [:div e]))
  )

(defmethod compile-element ::c-comp/literal-attributes
  [[tag attrs & rest-content]]
  `(let [tag# (name ~tag)]
     (if (get c-runtime/void-tags tag#)
       (str (str "<" tag#) ~(compile-attrs attrs) " />")
       (str (str "<" tag#) ~(compile-attrs attrs) ">"
            ~@(compile-seq rest-content)
            (str "</" tag# ">")))))

(comment
  (require '[ewen.inccup.compiler :refer [h]])
  (let [e :p] (h [e {:e "e"}]))
  )

(defmethod compile-element ::c-comp/map-attributes
  [[tag attrs & rest-content]]
  `(let [tag# (name ~tag)]
     (if (get c-runtime/void-tags tag#)
       (str (str "<" tag#) (runtime/render-attrs ~attrs) " />")
       (str (str "<" tag#) (runtime/render-attrs ~attrs) ">"
            ~@(compile-seq rest-content)
            (str "</" tag# ">")))))

(comment
  (require '[ewen.inccup.compiler :refer [h]])
  (let [e :p f "e"] (h [e {:e f}]))
  )

(defmethod compile-element ::c-comp/no-attributes
  [[tag & content]]
  (compile-element (apply vector tag {} content)))

(comment
  (require '[ewen.inccup.compiler :refer [h]])
  (c-comp/element-compile-strategy '[a])
  (let [e :p] (h [e]))
  )

(defmethod compile-element :default
  [[tag first-content & rest-content]]
  `(let [tag# (name ~tag)
         attrs# ~first-content]
     (if (map? attrs#)
       (if (get c-runtime/void-tags tag#)
         (str "<" tag# (runtime/render-attrs attrs#) " />")
         (str "<" tag# (runtime/render-attrs attrs#) ">"
              ~@(compile-seq rest-content)
              "</" tag# ">"))
       (if (get c-runtime/void-tags tag#)
         (str "<" tag# " />")
         (str "<" tag# ">"
              ~@(compile-seq (cons first-content rest-content))
              "</" tag# ">")))))

(comment
  (require '[ewen.inccup.compiler :refer [h]])
  (c-comp/element-compile-strategy '[a b])
  (let [e :p f {:e "e"}] (h [e f]))
  (let [e :p f "r"] (h [e f]))
  )

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
    (c-comp/literal? expr) expr
    (seq? expr) `(runtime/form->string ~expr)
    :else `(runtime/form->string ~expr)))

(defmacro compile-string [forms]
  (compile-dispatch forms))
