(ns ewen.inccup.string.compiler
  "Internal functions for compilation."
  (:require [ewen.inccup.common.util :as util]
            [ewen.inccup.common.runtime :as c-runtime]
            [ewen.inccup.common.compiler :as c-comp]
            [ewen.inccup.string.runtime :as runtime])
  (:import [clojure.lang IPersistentVector ISeq Named]
           [ewen.inccup.common.compiler DynamicLeaf]
           [ewen.inccup.string.runtime InccupString]))

(defn get-dynamic-form [dynamic dyn-leaf]
  (->> (.-index dyn-leaf)
       (get dynamic)
       :form))

(defn compile-attrs [dynamic attrs]
  (if (instance? DynamicLeaf attrs)
    `(runtime/render-attrs ~(get-dynamic-form dynamic attrs))
    (runtime/render-attrs attrs)))

(declare collapse-strings)

(defn collapse-reducer [collapsed form]
  (if (string? form)
    (let [last-collapsed (peek collapsed)]
      (if (string? last-collapsed)
        (conj (pop collapsed) (str last-collapsed form))
        (conj collapsed form)))
    (if (= 'clojure.core/str (first form))
      (reduce collapse-reducer collapsed (collapse-strings (rest form)))
      (conj collapsed form))))

(defn collapse-strings [forms]
  (reduce collapse-reducer [] forms))

(defn literal->string [dynamic x]
  (cond
    (instance? InccupString x) (str x)
    (keyword? x) (literal->string (name x))
    (vector? x)
    (let [[tag attrs & content] x]
      (cond
        (instance? DynamicLeaf tag)
        `(str
          (let [tag# (name ~(get-dynamic-form dynamic tag))]
            (if (get c-runtime/void-tags tag#)
              (str "<" tag# ~(compile-attrs dynamic attrs) " />")
              (str "<" tag# ~(compile-attrs dynamic attrs) ">"
                   ~@content
                   "</" tag# ">"))))
        (get c-runtime/void-tags tag)
        `(str ~(str "<" tag) ~(compile-attrs dynamic attrs) " />")
        :else
        `(str "<" ~tag ~(compile-attrs dynamic attrs) ">"
              ~@content
              ~(str "</" tag ">"))))
    (instance? DynamicLeaf x) `(runtime/form->string
                                ~(get-dynamic-form dynamic x))
    :else (-> x util/escape-string runtime/wrap-text)))

(defn compile-string [forms]
  (let [[static dynamic]
        (binding [c-comp/*dynamic-forms* []]
          [(c-comp/compile-dispatch forms []) c-comp/*dynamic-forms*])
        compiled (c-comp/walk-static c-comp/handle-void-tags
                                     (partial literal->string dynamic)
                                     static)]
    `(runtime/->InccupString (str ~@(collapse-strings (rest compiled))))))

(comment
  (require '[ewen.inccup.compiler :refer [h]])
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
  (let [e "ee"] (h [:div ^String e "t"]))
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
