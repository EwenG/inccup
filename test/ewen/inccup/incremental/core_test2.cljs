(ns ewen.inccup.incremental.core-test2
  (:require [cljs.test :refer-macros [deftest testing is run-tests]]
            [ewen.inccup.compiler
             :refer-macros [with-opts register-tagged-literal!]]
            [ewen.inccup.incremental.vdom2 :as vdom2 :refer [render!]]
            [ewen.inccup.incremental.component :refer [Component]]
            [cljs.pprint :refer [pprint] :refer-macros [pp]]
            [goog.array]
            [goog.dom])
  (:refer-clojure :exclude [-equiv]))

(register-tagged-literal! h)

(set-print-fn! #(.log js/console %))

(defn new-root []
  (let [old-root (.getElementById js/document "root")
        new-root (goog.dom/createDom "div" #js {:id "root"})]
    (if old-root
      (goog.dom/replaceNode new-root old-root)
      (goog.dom/appendChild (.-body js/document) new-root))
    new-root))

(defn root []
  (.querySelector js/document "#root"))

(let [e #h [:p]]
  (def def1 #h [:div {} e]))

(comment
  (render! (new-root) def1)
  )
