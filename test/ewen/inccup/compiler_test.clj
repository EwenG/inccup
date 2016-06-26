(ns ewen.inccup.compiler-test
  (:require [clojure.test :refer [deftest testing is run-tests]]
            [ewen.inccup.utils-test :as utils]
            [ewen.inccup.compiler :as comp :refer [h]]
            [clojure.data.xml :as xml]))

(defonce ^:dynamic *repl-env* nil)

(defmacro set-output-mode! [output-mode]
  (alter-var-root #'comp/*cljs-output-mode* (constantly output-mode))
  nil)

(defmacro multi-defn [name args & body]
  `(defn ~name ~args
     (if (= :string ewen.inccup.compiler-test/*cljs-output-mode*)
       (do
         (set-output-mode! :string)
         ~@body)
       (do
         (set-output-mode! :incremental)
         ~@body))))

(defn run-cljs-test
  [repl-env comp-fn init-params & rest-params]
  (utils/with-eval
    repl-env
    (utils/cljs-test-quote
     (set! comp-test/*comp*
           (vdom/render! (utils/new-root) ~comp-fn ~@init-params))))
  (let [string-template (utils/with-eval
                          repl-env
                          (utils/cljs-test-quote
                           (binding [comp-test/*cljs-output-mode* :string]
                             (comp-test/set-output-mode! :string)
                             (str (~comp-fn ~@init-params)))))
        string-inc (utils/with-eval
                     repl-env
                     (utils/cljs-test-quote
                      (do
                        (comp-test/set-output-mode! :incremental)
                        (utils/node-to-string (utils/root)))))]
    (is (= (xml/parse (java.io.StringReader. string-template))
           (xml/parse (java.io.StringReader. string-inc)))))
  (doseq [params rest-params]
    (utils/with-eval
      repl-env
      (utils/cljs-test-quote
       (vdom/update! comp-test/*comp* ~@params)))
    (let [string-template (utils/with-eval
                            repl-env
                            (utils/cljs-test-quote
                             (binding [comp-test/*cljs-output-mode* :string]
                               (comp-test/set-output-mode! :string)
                               (str (~comp-fn ~@params)))))
          string-inc (utils/with-eval
                       repl-env
                       (utils/cljs-test-quote
                        (do
                          (comp-test/set-output-mode! :incremental)
                          (utils/node-to-string (utils/root)))))]
      (is (= (xml/parse (java.io.StringReader. string-template))
             (xml/parse (java.io.StringReader. string-inc)))))))

(deftest simple1
  (run-cljs-test *repl-env*
                 (utils/cljs-test-quote comp-test/simple1)
                 ["e"] ["f"] ["g"]))

(deftest simple2
  (run-cljs-test *repl-env*
                 (utils/cljs-test-quote comp-test/simple2)
                 [:p {:e "e"} "t"]
                 [:p {:class "c2"} "t"]
                 [:p {:class "c2" :e "e"} "t"]))

(deftest simple3
  (run-cljs-test *repl-env*
                 (utils/cljs-test-quote comp-test/simple3)
                 ["e"] ["f"] [{:id "i"}]))

(deftest simple4
  (run-cljs-test *repl-env*
                 (utils/cljs-test-quote comp-test/simple4)
                 [] [] []))

(deftest simple5
  (run-cljs-test *repl-env*
                 (utils/cljs-test-quote comp-test/simple5)
                 [:p] [:div] [:p] [:input] [:p]))

(deftest list1
  (run-cljs-test *repl-env*
                 (utils/cljs-test-quote comp-test/list1)
                 [(utils/cljs-test-quote (list 1 2)) nil]
                 [(utils/cljs-test-quote (list 1 3))
                  (utils/cljs-test-quote (comp/h [:div]))]
                 [(utils/cljs-test-quote (list 4)) {:class "c"}]
                 [(utils/cljs-test-quote (list 4)) {:class "e"}]))

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
  (list1)
  )
