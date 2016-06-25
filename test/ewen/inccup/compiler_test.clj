(ns ewen.inccup.compiler-test
  (:require [clojure.test :refer [deftest testing is run-tests]]
            [ewen.inccup.utils-test :as utils]
            [ewen.inccup.compiler :as comp :refer [h]]
            [ewen.inccup.compiler :refer [*cljs-output-mode*]]
            [clojure.data.xml :as xml]))

(defonce ^:dynamic *repl-env* nil)

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

(defmacro with-eval [repl-env & body]
  `(utils/eval-js
    ~repl-env
    (utils/compile-cljs ~@body)))

(defn run-cljs-test
  [repl-env comp-fn comp-fn-string init-params & rest-params]
  (with-eval
    repl-env
    (utils/cljs-test-quote
     (set! comp-test/*comp*
           (vdom/render! (utils/new-root) ~comp-fn ~@init-params))))
  (let [string-inc (with-eval
                     repl-env
                     (utils/cljs-test-quote
                      (utils/node-to-string (utils/root))))
        string-template (with-eval
                          repl-env
                          (utils/cljs-test-quote
                           (str (~comp-fn-string ~@init-params))))]
    (is (= (xml/parse (java.io.StringReader. string-template))
           (xml/parse (java.io.StringReader. string-inc)))))
  (doseq [params rest-params]
    (with-eval
      repl-env
      (utils/cljs-test-quote
       (vdom/update! comp-test/*comp* ~comp-fn ~@params)))
    (let [string-inc (with-eval
                       repl-env
                       (utils/cljs-test-quote
                        (utils/node-to-string (utils/root))))
          string-template (with-eval
                            repl-env
                            (utils/cljs-test-quote
                             (str (~comp-fn-string ~@params))))]
      (is (= (xml/parse (java.io.StringReader. string-template))
             (xml/parse (java.io.StringReader. string-inc)))))))

(deftest simple1
  (testing "simple1"
    (run-cljs-test *repl-env*
                   (utils/cljs-test-quote comp-test/simple1)
                   (utils/cljs-test-quote comp-test/simple1-string)
                   ["e"] ["f"] ["g"])))

(deftest simple2
  (testing "simple2"
    (run-cljs-test *repl-env*
                   (utils/cljs-test-quote comp-test/simple2)
                   (utils/cljs-test-quote comp-test/simple2-string)
                   [:p {:e "e"} "t"]
                   [:p {:class "c2"} "t"]
                   [:p {:class "c2" :e "e"} "t"])))

(deftest simple3
  (testing "simple3"
    (run-cljs-test *repl-env*
                   (utils/cljs-test-quote comp-test/simple3)
                   (utils/cljs-test-quote comp-test/simple3-string)
                   ["e"] ["f"] [{:id "i"}])))

(deftest simple4
  (testing "simple4"
    (run-cljs-test *repl-env*
                   (utils/cljs-test-quote comp-test/simple4)
                   (utils/cljs-test-quote comp-test/simple4-string)
                   [] [] [])))

(deftest simple5
  (testing "simple5"
    (run-cljs-test *repl-env*
                   (utils/cljs-test-quote comp-test/simple5)
                   (utils/cljs-test-quote comp-test/simple5-string)
                   [:p] [:div] [:p] [:input] [:p])))

(defn test-compiler [repl-env]
  (binding [*repl-env* repl-env]
    (run-tests 'ewen.inccup.compiler-test)))

(comment
  (require '[ewen.replique.server-cljs :refer [repl-env]])
  (alter-var-root #'*repl-env* (constantly repl-env))

  (simple1)
  (simple2)
  (simple3)
  (simple4)
  (simple5)
  )
