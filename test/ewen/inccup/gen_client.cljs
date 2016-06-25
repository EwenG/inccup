(ns ewen.inccup.gen-client
  (:require [goog.dom]
            [goog.object]
            [ewen.inccup.utils-test]
            [ewen.inccup.compiler :as comp :refer-macros [h]]
            [ewen.inccup.incremental.vdom :as vdom]))

(set-print-fn! #(.log js/console %))

(def string-fn-cljs nil)
(def component-fn nil)
(def component nil)

(comment
  (def comp1 (fn [VPRuP]
               (h [:output {:s- "G"} [:tfoot {} [VPRuP {}]]])))
  (def comp1-s (fn [VPRuP]
                 (h [:output {:s- "G"} [:tfoot {} [VPRuP {}]]]
                    {::comp/output-mode :string})))

  (goog.object/set (new-root-string) "innerHTML" (comp1-s :hb))
  (vdom/render! (new-root-comp) comp1 :hb)

  (def comp2 (fn [sMnsJXGuK]
               (h [sMnsJXGuK {:Y ""} [sMnsJXGuK {:p "P"}]])))
  (def comp2-s (fn [sMnsJXGuK]
                 (h [sMnsJXGuK {:Y ""} [sMnsJXGuK {:p "P"}]]
                    {::comp/output-mode :string})))

  (goog.object/set (new-root-string) "innerHTML" (comp2-s :A))
  (vdom/render! (new-root-comp) comp2 :A)

  (roots-equal?)

  )
