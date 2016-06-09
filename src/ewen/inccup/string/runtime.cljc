(ns ewen.inccup.string.runtime
  (:require [ewen.inccup.common.util :as util]))

(deftype InccupString [^String s]
  Object
  (^String toString [this] s)
  (^boolean equals [this other]
   (and (instance? InccupString other)
        (= s (.toString other)))))

(defn- attribute [k v]
  (str " " (name k) "=\"" (util/escape-string v) "\""))

(defn- render-attr [[k v]]
  (cond
    (not v) ""
    (not (keyword? k)) ""
    :else (attribute k v)))

(defn render-attrs
  [attrs]
  (apply str (map render-attr attrs)))

(defn wrap-text [text]
  (if (= "" text)
    text
    (str "<!--inccup/text-start-->" text "<!--inccup/text-end-->")))

(defn form->string [form]
  (cond
    (instance? InccupString form) (str form)
    (seq? form) (apply str (map form->string form))
    :else (-> form util/escape-string wrap-text)))
