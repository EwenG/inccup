(ns ewen.inccup.common.gen-client
  (:require [goog.dom]
            [goog.object]
            [ewen.inccup.compiler :as comp :refer-macros [h]]
            [ewen.inccup.incremental.vdom :as vdom]))

(set-print-fn! #(.log js/console %))

(defn node= [node1 node2]
  (let [equal-nodes (volatile! true)]
    (if (not= (.-nodeName node1) (.-nodeName node2))
      (do
        (prn (.-nodeName node1) " " (.-nodeName node2))
        (vreset! equal-nodes false))
      (let [attrs1 (.-attributes node1)
            attrs2 (.-attributes node2)]
        (when (not (and (nil? attrs1) (nil? attrs2)))
          (if (not= (.-length attrs1) (.-length attrs2))
            (do
              #_(prn (.-length attrs1) " " (.-length attrs2))
              (vreset! equal-nodes false))
            (loop [index 0
                   l (.-length attrs1)]
              (when (and @equal-nodes (< index l))
                (when (not= (.-value (aget attrs1 index))
                            (.getAttribute
                             node2 (.-name (aget attrs1 index))))
                  (do
                    #_(prn (.-value (aget attrs1 index)) " "
                           (.getAttribute
                            node2 (.-name (aget attrs1 index))))
                    (vreset! equal-nodes false)))
                (recur (inc index) l)))))))
    (when @equal-nodes
      (let [children1 (.-childNodes node1)
            children2 (.-childNodes node2)]
        (if (not= (.-length children1) (.-length children2))
          (do
            #_(prn (.-length children1) " " (.-length children2))
            (vreset! equal-nodes false))
          (loop [index 0
                 l (.-length children1)]
            (when (and @equal-nodes (< index l))
              (when-not (node= (aget children1 index)
                               (aget children2 index))
                (vreset! equal-nodes false))
              (recur (inc index) l))))))
    @equal-nodes))

(def string-fn-cljs nil)
(def component nil)

(defn new-root-string []
  (let [old-root (.getElementById js/document "root-string")
        new-root (goog.dom/createDom "div" #js {:id "root-string"})]
    (if old-root
      (goog.dom/replaceNode new-root old-root)
      (goog.dom/appendChild (.-body js/document) new-root))
    new-root))

(defn root-string []
  (.querySelector js/document "#root-string"))

(defn new-root-comp []
  (let [old-root (.getElementById js/document "root-comp")
        new-root (goog.dom/createDom "div" #js {:id "root-comp"})]
    (if old-root
      (goog.dom/replaceNode new-root old-root)
      (goog.dom/appendChild (.-body js/document) new-root))
    new-root))

(defn root-comp []
  (.querySelector js/document "#root-comp"))

(defn roots-equal? []
  (assert (= 1
             (.-length (.-childNodes (root-string)))
             (.-length (.-childNodes (root-comp)))))
  (node= (.-firstChild (root-string)) (.-firstChild (root-comp))))

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
