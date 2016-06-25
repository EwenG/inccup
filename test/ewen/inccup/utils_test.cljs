(ns ewen.inccup.utils-test
  (:require [goog.dom]
            [ewen.inccup.common.util :as util])
  (:import [goog.dom TagIterator]
           [goog.iter StopIteration]))

(defn new-root []
  (let [old-root (.getElementById js/document "root")
        new-root (goog.dom/createDom "div" #js {:id "root"})]
    (if old-root
      (goog.dom/replaceNode new-root old-root)
      (goog.dom/appendChild (.-body js/document) new-root))
    new-root))

(defn root []
  (.querySelector js/document "#root"))

(def ^:dynamic *node-iterator* nil)
(def ^:dynamic *node-string* nil)
(def ^:dynamic *node-root* nil)

(defn node-to-string* []
  (let [node (.-node *node-iterator*)
        start-or-end (.-tagType *node-iterator*)]
    (cond (= 3 (.-nodeType node))
          (set! *node-string* (str *node-string*
                                   (-> (.-nodeValue node)
                                       util/escape-string)))
          (= 1 start-or-end)
          (let [node-name (-> (.-nodeName node)
                              clojure.string/lower-case)
                attrs (.-attributes node)
                attrs-length (.-length attrs)
                attrs-string (volatile! "")]
            (loop [index 0]
              (when (< index attrs-length)
                (vswap! attrs-string
                        (fn [s]
                          (str s " "
                               (.-name (aget attrs index))
                               "="
                               (str "\""
                                    (.-value (aget attrs index))
                                    "\""))))
                (recur (inc index))))
            (when (not (identical? node *node-root*))
              (set! *node-string* (str *node-string* "<" node-name
                                       @attrs-string ">"))))
          (= -1 start-or-end)
          (let [node-name (-> (.-nodeName node)
                              clojure.string/lower-case)]
            (when (not (identical? node *node-root*))
              (set! *node-string* (str *node-string* "</" node-name ">"))))
          :else (throw (js/Error. "Invalid tag iterator state" )))))

(defn node-to-string [node]
  (binding [*node-iterator* (TagIterator. node)
            *node-string* ""
            *node-root* node]
    (try
      (while true
        (.next *node-iterator*)
        (node-to-string*))
      (catch :default e
        *node-string*))))




#_(defn node= [node1 node2]
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

#_(defn new-root-string []
  (let [old-root (.getElementById js/document "root-string")
        new-root (goog.dom/createDom "div" #js {:id "root-string"})]
    (if old-root
      (goog.dom/replaceNode new-root old-root)
      (goog.dom/appendChild (.-body js/document) new-root))
    new-root))

#_(defn root-string []
  (.querySelector js/document "#root-string"))

#_(defn new-root-comp []
  (let [old-root (.getElementById js/document "root-comp")
        new-root (goog.dom/createDom "div" #js {:id "root-comp"})]
    (if old-root
      (goog.dom/replaceNode new-root old-root)
      (goog.dom/appendChild (.-body js/document) new-root))
    new-root))

#_(defn root-comp []
  (.querySelector js/document "#root-comp"))

#_(defn roots-equal? []
  (assert (= 1
             (.-length (.-childNodes (root-string)))
             (.-length (.-childNodes (root-comp)))))
  (node= (.-firstChild (root-string)) (.-firstChild (root-comp))))
