(ns ewen.inccup.incremental.core-test-macros
  (:require [cljs.tagged-literals :refer [*cljs-data-readers*]]))

(alter-var-root #'*cljs-data-readers* assoc 'inccup/ComponentValue
                (fn [val]
                  `(ewen.inccup.incremental.core-test/ComponentValue.
                    ~val)))
