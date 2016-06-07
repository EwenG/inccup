(ns ewen.inccup.common.gen-client
  (:require [ewen.inccup.compiler :as comp]
            [ewen.inccup.common.spec]
            [cljs.spec :as spec]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen
             :include-macros true]
            [clojure.test.check.properties :as prop
             :include-macros true]))
