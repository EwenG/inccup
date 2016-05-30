(ns ewen.inccup.string.runtime
  (:require [ewen.inccup.util :as util #?@(:cljs [:refer [RawString]])])
  #?(:clj (:import [ewen.inccup.util RawString])))

(defn- xml-mode? []
  (get #{:xml :xhtml} util/*html-mode*))

(defn- xml-attribute [k v]
  (str " " (name k) "=\"" (util/escape-string v) "\""))

(defn- render-attr [[k v]]
  (cond
    (true? v)
    (if (xml-mode?)
      (xml-attribute k k)
      (str " " (name k)))
    (not v)
    ""
    :else
    (xml-attribute k v)))

(defn render-attrs
  [attrs]
  (apply str (map render-attr attrs)))

(defn form->string [form]
  (cond
    (instance? RawString form) (str form)
    (seq? form) (apply str (map form->string form))
    :else (util/escape-string form)))
