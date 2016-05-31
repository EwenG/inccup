(ns ewen.inccup.string.runtime
  (:require [ewen.inccup.common.util :as util
             #?@(:cljs [:refer [RawString]])])
  #?(:clj (:import [ewen.inccup.common.util RawString])))

(defn- attribute [k v]
  (str " " (name k) "=\"" (util/escape-string v) "\""))

(def void-tags
  "A list of elements that must be rendered without a
  closing tag."
  #{"area" "base" "br" "col" "command" "embed" "hr" "img" "input"
    "keygen" "link" "meta" "param" "source" "track" "wbr"})

(defn- render-attr [[k v]]
  (if (not v)
    ""
    (attribute k v)))

(defn render-attrs
  [attrs]
  (apply str (map render-attr attrs)))

(defn form->string [form]
  (cond
    (instance? RawString form) (str form)
    (seq? form) (apply str (map form->string form))
    :else (util/escape-string form)))
