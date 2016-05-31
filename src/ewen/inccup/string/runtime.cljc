(ns ewen.inccup.string.runtime
  (:require [ewen.inccup.common.util :as util
             #?@(:cljs [:refer [RawString]])])
  #?(:clj (:import [ewen.inccup.common.util RawString])))

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

(defn form->string [form]
  (cond
    (instance? RawString form) (str form)
    (seq? form) (apply str (map form->string form))
    :else (util/escape-string form)))
