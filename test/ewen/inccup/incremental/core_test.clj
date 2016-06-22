(ns ewen.inccup.incremental.core-test
  (:require [ewen.inccup.compiler :refer [*cljs-output-mode*]]))

(defmacro set-output-mode! [output-mode]
  (alter-var-root #'*cljs-output-mode* (constantly output-mode))
  nil)

(defmacro multi-defn [name args & body]
  `(do
     (set-output-mode! :incremental)
     (defn ~name ~args ~@body)
     (set-output-mode! :string)
     (defn ~(symbol (str name "-string")) ~args ~@body)
     (set-output-mode! :incremental)))
