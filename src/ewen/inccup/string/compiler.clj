(ns ewen.inccup.string.compiler
  "Internal functions for compilation."
  (:require [ewen.inccup.common.util :as util]
            [ewen.inccup.common.runtime :as c-runtime]
            [ewen.inccup.common.compiler :as c-comp]
            [ewen.inccup.string.runtime :as runtime])
  (:import [clojure.lang IPersistentVector ISeq Named]
           [ewen.inccup.common.compiler DynamicLeaf]))

(deftype RawString [^String s]
  Object
  (^String toString [this] s)
  (^boolean equals [this other]
   (and (instance? RawString other)
        (= s  (.toString other)))))

(defn raw-string
  "Wraps a string to an object that will be pasted to HTML without
  escaping."
  ([] (RawString. ""))
  ([x] (RawString. x))
  ([x & xs] (RawString. (apply str x xs))))

(defn raw-string?
  "Returns true if x is a RawString"
  [x]
  (instance? RawString x))

(defn get-dynamic-form [dynamic dyn-leaf]
  (->> (.-index dyn-leaf)
       (get dynamic)
       :form))

(defn compile-attrs [dynamic attrs]
  (if (instance? DynamicLeaf attrs)
    `(runtime/render-attrs ~(get-dynamic-form dynamic attrs))
    (runtime/render-attrs attrs)))

(defn literal->string [dynamic x]
  (cond
    (instance? RawString x) (str x)
    (keyword? x) (util/escape-string (name x))
    (vector? x)
    (let [[tag attrs & content] x]
      (cond
        (instance? DynamicLeaf tag)
        `(let [tag# (name ~(get-dynamic-form dynamic tag))]
           (if (get c-runtime/void-tags tag#)
             (str "<" tag# ~(compile-attrs dynamic attrs) " />")
             (str "<" tag# ~(compile-attrs dynamic attrs) ">"
                  ~@content
                  "</" tag# ">")))
        (get c-runtime/void-tags tag)
        `(str ~(str "<" tag) ~(compile-attrs dynamic attrs) " />")
        :else
        `(str "<" ~tag ~(compile-attrs dynamic attrs) ">"
             ~@content
             ~(str "</" tag ">"))))
    (map? x) `(cljs.core/js-obj
               ~@(interleave (map name (keys x))
                             (map util/escape-string (vals x))))
    (instance? DynamicLeaf x) `(runtime/form->string
                                ~(get-dynamic-form dynamic x))
    :else (util/escape-string x)))

(defmacro compile-string [forms]
  (let [[static dynamic]
        (binding [c-comp/*dynamic-forms* []]
          [(c-comp/compile-dispatch forms []) c-comp/*dynamic-forms*])]
    (c-comp/walk-static c-comp/handle-void-tags
                        (partial literal->string dynamic)
                        static)))

(comment
  (require '[ewen.inccup.compiler :refer [h]])
  (h [:div])
  )

(comment
  (require '[ewen.inccup.compiler :refer [h]])
  (h [:div {:e "e"}])
  )

(comment
  (require '[ewen.inccup.compiler :refer [h]])
  (let [e "ee"] (h [:div {:e e}]))
  )

(comment
  (require '[ewen.inccup.compiler :refer [h]])
  (let [e "ee"] (h [:div ^String e]))
  )

(comment
  (require '[ewen.inccup.compiler :refer [h]])
  (binding [c-runtime/*attrs-or-first-child* nil]
    (let [e "ee"] (h [:div e])))
  (binding [c-runtime/*attrs-or-first-child* nil]
    (let [e {:e "e"}] (h [:div e])))
  )

(comment
  (require '[ewen.inccup.compiler :refer [h]])
  (let [e :p] (h [e {:e "e"}]))
  )

(comment
  (require '[ewen.inccup.compiler :refer [h]])
  (let [e :p f "e"] (h [e {:e f}]))
  )


(comment
  (require '[ewen.inccup.compiler :refer [h]])
  (c-comp/element-compile-strategy '[a])
  (let [e :p] (h [e]))
  )

(comment
  (require '[ewen.inccup.compiler :refer [h]])
  (c-comp/element-compile-strategy '[a b] [])
  (binding [c-runtime/*attrs-or-first-child* nil]
    (let [e :p f {:e "e"}] (h [e f])))
  (binding [c-runtime/*attrs-or-first-child* nil]
    (let [e :p f "r"] (h [e f])))
  )
