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

(defn reconcialiation-comp [x]
  (if (= :string *output-mode*)
    (h [:div {} x] {::comp/output-mode :string})
    (h [:div {} x])))

(comment
  (let [comp (vdom/render! (utils/new-root) reconcialiation-comp (clojure.core/list))]
    (reset! *key-counter* 0)
    (vdom/update! comp reconcialiation-comp (clojure.core/list "" "" (ewen.inccup.gen-client/make-component-with-key) (ewen.inccup.gen-client/make-component-with-key)))
    (reset! *key-counter* 0)
    (vdom/update! comp reconcialiation-comp (clojure.core/list (ewen.inccup.gen-client/make-component) ""))

    )

  )
