(ns ewen.inccup.repl-test
  (:require [cljs.repl]
            [cljs.build.api]
            [cljs.repl.browser]
            [ewen.inccup.common.gen]))

;;lein run -m ewen.inccup.repl-test/run-cljs-tests

(defn run-cljs-tests [test-nb-runs]
  (let [test-nb-runs (Integer/parseInt test-nb-runs)]
    (cljs.build.api/build "test"
                          {:main 'ewen.inccup.repl-test
                           :output-to "out-test/main.js"
                           :output-dir "out-test"
                           :verbose true})

    (spit "out-test/index.html"
          "<html>
    <body>
        <script type=\"text/javascript\" src=\"out-test/main.js\"></script>
    </body>
</html>")

    (def repl-env (cljs.repl.browser/repl-env
                   :src "test/"
                   :static-dir ["." "out-test"]))

    (cljs.repl/repl repl-env
                    :output-dir "out-test"
                    :init #(ewen.inccup.common.gen/run-cljs-tests
                            repl-env test-nb-runs))))
