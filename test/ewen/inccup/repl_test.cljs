(ns ewen.inccup.repl-test
  (:require [clojure.browser.repl :as repl]
            [ewen.inccup.common.gen-client]))

(defonce conn
  (repl/connect "http://localhost:9000/repl"))

(set-print-fn! #(.log js/console %))
