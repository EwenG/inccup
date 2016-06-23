(ns ewen.inccup.common.compiler-test
  (:require [clojure.test :refer [deftest testing is run-tests]]
            [ewen.inccup.common.utils-test :as utils]
            [ewen.inccup.compiler :as comp :refer [h]]
            [cljs.analyzer.api :as ana-api]
            [cljs.compiler.api :as comp-api]
            [cljs.repl]
            [clojure.data.xml :as xml]
            [backtick]))

(defmacro with-eval [& body]
  `(utils/eval-js
    repl-env
    (utils/compile-cljs ~@body)))

(defn run-cljs-test
  [comp-fn comp-fn-string init-params & rest-params]
  (with-eval
    (utils/cljs-test-quote
     (set! comp/*comp*
           (vdom/render! (utils/new-root) ~comp-fn ~@init-params))))
  (let [string-inc (with-eval
                     (utils/cljs-test-quote
                      (utils/node-to-string (utils/root))))
        string-template (with-eval
                          (utils/cljs-test-quote
                           (str (~comp-fn-string ~@init-params))))]
    (is (= (xml/parse (java.io.StringReader. string-template))
           (xml/parse (java.io.StringReader. string-inc)))))
  (doseq [params rest-params]
    (with-eval
      (utils/cljs-test-quote
       (vdom/update! comp-test/*comp* ~comp-fn ~@params)))
    (let [string-inc (with-eval
                       (utils/cljs-test-quote
                        (utils/node-to-string (utils/root))))
          string-template (with-eval
                            (utils/cljs-test-quote
                             (str (~comp-fn-string ~@params))))]
      (is (= (xml/parse (java.io.StringReader. string-template))
             (xml/parse (java.io.StringReader. string-inc)))))))

(deftest simple1
  (testing "simple1"
    (run-cljs-test (utils/cljs-test-quote comp-test/simple1)
                   (utils/cljs-test-quote comp-test/simple1-string)
                   ["e"] ["f"] ["g"])))

(deftest simple2
  (testing "simple2"
    (run-cljs-test (utils/cljs-test-quote comp-test/simple2)
                   (utils/cljs-test-quote comp-test/simple2-string)
                   [:p {:e "e"} "t"]
                   [:p {:class "c2"} "t"]
                   [:p {:class "c2" :e "e"} "t"])))

(deftest simple3
  (testing "simple3"
    (run-cljs-test (utils/cljs-test-quote comp-test/simple3)
                   (utils/cljs-test-quote comp-test/simple3-string)
                   ["e"] ["f"] [{:id "i"}])))

(deftest simple4
  (testing "simple4"
    (run-cljs-test (utils/cljs-test-quote comp-test/simple4)
                   (utils/cljs-test-quote comp-test/simple4-string)
                   [] [] [])))

(deftest simple5
  (testing "simple5"
    (run-cljs-test (utils/cljs-test-quote comp-test/simple5)
                   (utils/cljs-test-quote comp-test/simple5-string)
                   [:p] [:div] [:p] [:input] [:p])))


(comment
  (require '[ewen.replique.server-cljs :refer [repl-env]])

  (simple1)
  (simple2)
  (simple3)
  (simple4)
  (simple5)
  )
