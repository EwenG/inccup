(ns ewen.inccup.gen-reconciliation
  (:require [ewen.inccup.utils-test :as utils]
            [ewen.inccup.compiler :as comp :refer [h]]
            [clojure.test :refer [is]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.data.xml :as xml]))

(def comp-gen
  (gen/one-of
   [(gen/return
     (utils/cljs-test-quote (gen-client/make-component)))
    (gen/return
     (utils/cljs-test-quote (gen-client/make-component-with-key)))]))

(comment
  (gen/sample comp-gen)
  )

(def leaf-gen (gen/one-of [comp-gen gen/string-ascii]))

(comment
  (gen/sample leaf-gen)
  )

(def list-gen
  (gen/recursive-gen
   (fn [child-gen]
     (->> (gen/list child-gen)
          (gen/fmap (fn [l] `(list ~@l)))))
   leaf-gen))

(comment
  (gen/sample list-gen)
  )

(defn test-prop [repl-env]
  ;; Reset the key counter to 0
  (utils/with-eval
    repl-env
    (utils/cljs-test-quote
     (c/reset! gen-client/*key-counter* 0)))

  (let [children-list (gen/generate list-gen)]

    ;; Render the clojurescript component
    (utils/with-eval
      repl-env
      (utils/cljs-test-quote
       (set! gen-client/component
             (vdom/render! (utils/new-root)
                           gen-client/reconcialiation-comp
                           ~children-list))))

    ;; Check that the clojurescript generated string is equal to the
    ;; rendered component
    (let [string-template (utils/with-eval
                            repl-env
                            (utils/cljs-test-quote
                             (binding [gen-client/*output-mode* :string]
                               (str (gen-client/reconcialiation-comp
                                     ~children-list)))))
          string-inc (utils/with-eval
                       repl-env
                       (utils/cljs-test-quote
                        (utils/node-to-string (utils/root))))]
      (is (= (xml/parse (java.io.StringReader. string-template))
             (xml/parse (java.io.StringReader. string-inc))))))

  (prop/for-all
   [children-list list-gen]

   ;; Reset the key counter to 0
   (utils/with-eval
     repl-env
     (utils/cljs-test-quote
      (c/reset! gen-client/*key-counter* 0)))

   ;; Update the clojurescript component
   (utils/with-eval
     repl-env
     (utils/cljs-test-quote
      (vdom/update! gen-client/component
                    gen-client/reconcialiation-comp
                    ~children-list)))

   ;; Check that the clojurescript generated string is equal to the
   ;; updated component
   (let [string-template (utils/with-eval
                           repl-env
                           (utils/cljs-test-quote
                            (binding [gen-client/*output-mode* :string]
                              (str (gen-client/reconcialiation-comp
                                    ~children-list)))))
         string-inc (utils/with-eval
                      repl-env
                      (utils/cljs-test-quote
                       (utils/node-to-string (utils/root))))]
     #_(prn (utils/cljs-test-quote
           (vdom/update! gen-client/component
                         gen-client/reconcialiation-comp
                         ~children-list)))
     #_(prn string-template)
     #_(prn string-inc)
     (is (= (xml/parse (java.io.StringReader. string-template))
            (xml/parse (java.io.StringReader. string-inc)))))

   true))

(defn run-cljs-tests [repl-env test-nb-runs]
  (tc/quick-check
   test-nb-runs (gen/no-shrink (test-prop repl-env))))

(comment
  (require '[ewen.replique.server-cljs :refer [repl-env]])

  (utils/with-eval
    repl-env
    (utils/cljs-test-quote
     (set! gen-client/component
           (vdom/render! (utils/new-root)
                         gen-client/reconcialiation-comp
                         :string (list "g" "r")))))

  (tc/quick-check 100 (gen/no-shrink (test-prop repl-env)))

  (ewen.inccup.incremental.vdom/update!
   ewen.inccup.gen-client/component
   ewen.inccup.gen-client/reconcialiation-comp (clojure.core/list))
  )
