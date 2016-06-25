(ns ewen.inccup.tests-runner
  (:require [clojure.browser.repl :as repl]
            [ewen.inccup.gen-client]
            [ewen.inccup.compiler-test]))

(defonce conn
  (repl/connect "http://localhost:9000/repl"))

(set-print-fn! #(.log js/console %))
