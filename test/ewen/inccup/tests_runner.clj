(ns ewen.inccup.tests-runner
  (:require [cljs.repl]
            [cljs.build.api]
            [cljs.repl.browser]
            [ewen.inccup.gen]
            [ewen.inccup.gen-reconciliation]
            [ewen.inccup.compiler-test]))

(defn init-browser-repl [repl-init-fn & args]
  (cljs.build.api/build "test"
                        {:main 'ewen.inccup.tests-runner
                         :output-to "out-test/main.js"
                         :output-dir "out-test"
                         :verbose true})

  (spit "out-test/index.html"
        "<html>
    <body>
        <script type=\"text/javascript\" src=\"out-test/main.js\"></script>
    </body>
</html>")

  (let [repl-env (cljs.repl.browser/repl-env
                  :src "test/"
                  :static-dir ["." "out-test"])]

    (cljs.repl/repl repl-env
                    :output-dir "out-test"
                    :init #(apply repl-init-fn repl-env args))))

;;lein run -m ewen.inccup.tests-runner/run-gen-tests

(defn run-gen-tests [test-nb-runs]
  (let [test-nb-runs (Integer/parseInt test-nb-runs)]
    (init-browser-repl ewen.inccup.gen/run-cljs-tests test-nb-runs)))

(defn run-gen-reconciliation-tests [test-nb-runs]
  (let [test-nb-runs (Integer/parseInt test-nb-runs)]
    (init-browser-repl ewen.inccup.gen-reconciliation/run-cljs-tests
                       test-nb-runs)))

(defn run-compiler-tests []
  (init-browser-repl ewen.inccup.compiler-test/test-compiler))
