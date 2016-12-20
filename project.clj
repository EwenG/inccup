(defproject ewen/hiccup "1.0.0"
  :description "A fast library for rendering HTML in Clojure"
  :url "http://github.com/EwenG/hiccup"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :source-paths ["src"]
  :test-paths ["test"]
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [org.clojure/clojurescript "1.9.293"]]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]
                                  [backtick "0.3.3"]
                                  [org.clojure/data.xml "0.0.8"]]}}
  :gen-tests-runs-nb 30
  :aliases {"gen-tests" ["run" "-m"
                         "ewen.inccup.tests-runner/run-gen-tests"
                         :project/gen-tests-runs-nb]
            "gen-reconciliation-tests"
            ["run" "-m"
             "ewen.inccup.tests-runner/run-gen-reconciliation-tests"
             :project/gen-tests-runs-nb]
            "compiler-tests"
            ["run" "-m" "ewen.inccup.tests-runner/run-compiler-tests"]}
  :plugins [[lein-cljsbuild "1.1.5"]]
  :cljsbuild {
    :builds [{
        :source-paths ["src-cljs"]
              :compiler {
                         :output-to "target/cljs/main-o.js"
                         :optimizations :advanced
                         :pseudo-names true
                         :pretty-print true}}]})
