(ns ewen.inccup.gen-client
  (:require [goog.dom]
            [goog.object]
            [ewen.inccup.utils-test :as utils]
            [ewen.inccup.compiler :as comp :refer-macros [h]]
            [ewen.inccup.incremental.vdom :as vdom]))

(set-print-fn! #(.log js/console %))

(def string-fn-cljs nil)
(def component-fn nil)
(def component nil)



;Children reconciliation

(def ^:dynamic *output-mode* :incremental)
(def ^:dynamic *key-counter* (atom 0))

(defn make-component []
  (if (= :string *output-mode*)
    (h [:p] {::comp/output-mode :string})
    (h [:p])))

(defn make-component-with-key []
  (if (= :string *output-mode*)
    (h [:p] {::comp/output-mode :string})
    (comp/with-opts! {:key (swap! *key-counter* inc)}
      (h [:p]))))

(defn reconciliation-comp [x]
  (if (= :string *output-mode*)
    (h [:div {} x] {::comp/output-mode :string})
    (h [:div {} x])))

(comment
  (let [comp (vdom/render! (utils/new-root)
                           (fn [x]
                             (h [:div {}])))]
    (reset! *key-counter* 0)
    (vdom/update! comp (clojure.core/list (clojure.core/list (ewen.inccup.gen-client/make-component-with-key))))



    (defn cc [x] (h [:div {} x]))
    (def cc-comp (vdom/render! (utils/new-root) cc (clojure.core/list)))
    #_(reset! *key-counter* 0)
    #_(vdom/update! cc-comp (clojure.core/list (ewen.inccup.gen-client/make-component-with-key)))
    (reset! *key-counter* 0)
    (vdom/update! cc-comp (clojure.core/list (ewen.inccup.gen-client/make-component-with-key) (ewen.inccup.gen-client/make-component) (ewen.inccup.gen-client/make-component) (ewen.inccup.gen-client/make-component-with-key) "B,l11*CwvS}#(z" (ewen.inccup.gen-client/make-component) (ewen.inccup.gen-client/make-component) (ewen.inccup.gen-client/make-component-with-key) "B" (ewen.inccup.gen-client/make-component-with-key) "L" "H1>sQsCr@|6w," (ewen.inccup.gen-client/make-component) (ewen.inccup.gen-client/make-component-with-key) (ewen.inccup.gen-client/make-component) "DA@I6%mZY+_Yd7NY."))
    (reset! *key-counter* 0)
    (vdom/update! cc-comp (clojure.core/list (clojure.core/list (ewen.inccup.gen-client/make-component) "gkjfHj5,fs\\}~<i5c") (clojure.core/list) (clojure.core/list "=4rhZCb<" "|~ks,;mD," (ewen.inccup.gen-client/make-component)) (clojure.core/list)))
    (reset! *key-counter* 0)
    (vdom/update! cc-comp (clojure.core/list "`Ri;b" (ewen.inccup.gen-client/make-component) "_e>Pdz,Ep0Pu" (ewen.inccup.gen-client/make-component) (ewen.inccup.gen-client/make-component-with-key) "@JgHg=64jXI44*Nmw(HJ" ",a3f{QqUe5" "" (ewen.inccup.gen-client/make-component) "D\\'X" (ewen.inccup.gen-client/make-component) (ewen.inccup.gen-client/make-component) "f)" (ewen.inccup.gen-client/make-component-with-key) " "))

    )

  )
