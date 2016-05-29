(ns ewen.inccup.string.runtime
  (:require [ewen.inccup.util :as util #?@(:cljs [:refer [RawString]])])
  #?(:clj (:import [ewen.inccup.util RawString])))

(defn form->string [form]
  (cond
    (instance? RawString form) (str form)
    (seq? form) (apply str (map form->string form))
    :else (util/escape-string form)))
