(ns ewen.inccup.core
  (:require [goog.dom :as dom]
            [goog.object :as obj]
            [goog.array :as arr]
            [ewen.inccup.compiler :as comp :include-macros true]))

(defprotocol Html
  "Marker protocol")

(def parent-stack #js [])
(def sibling-index-stack #js [])
(def ^:dynamic *previous-node* nil)
(def ^:dynamic *node* nil)

(defn nth-child [vnode i]
  (-> (aget vnode "children")
      (aget i)))

(defn down-node! []
  (.push sibling-index-stack 0)
  (.push parent-stack *node*)
  (set! *node* (nth-child *node* 0))
  (set! *previous-node* nil))

(defn up-node! []
  (set! *previous-node* (arr/peek parent-stack))
  (.pop sibling-index-stack)
  (set! *node* (.pop parent-stack)))

(defn next-node! []
  (set! *previous-node* *node*)
  (let [index (inc (.pop sibling-index-stack))]
    (.push sibling-index-stack index)
    (set! *node* (nth-child *previous-node* index))))

(defn reconciliate-attrs! [new-attrs]
  (let [real-node (aget (or *node* #js {}) "real-node")
        attrs (aget (or *node* #js {}) "dyn-attrs")
        attrs-keys (obj/getKeys attrs)
        new-attrs-keys (obj/getKeys new-attrs)]
    (doseq [k attrs-keys]
      ;; Handle removed attrs
      (when-not (aget new-attrs k)
        (obj/remove attrs k)
        (.removeAttribute real-node k)))
    (doseq [k new-attrs-keys]
      ;; Update changes attrs
      (let [new-val (aget new-attrs k)]
        (when (not= new-val (aget attrs k))
          (aset attrs k new-val)
          (.setAttribute real-node k new-val))))))

(defn tag-key-matches? [new-tag new-key]
  (and (= (aget (or *node* #js {}) "tag") new-tag)
       (= (aget (or *node* #js {}) "key") new-key)))

(defn tag-matches? [new-tag]
  (= (aget (or *node* #js {}) "tag") new-tag))

(defn new-node [tag key attrs dyn-attrs]
  (let [real-node (dom/createDom tag attrs)]
    #js {:real-node real-node
         :tag tag :key key :dyn-attrs dyn-attrs
         :children #js [] :key-map #js {}}))

(defn replace-node! [new-node]
  (let [index (arr/peek sibling-index-stack)
        parent (arr/peek parent-stack)
        children (aget parent "children")
        node (nth-child parent index)
        real-node (aget (or node #js {}) "real-node")]
    (if (nth-child parent index)
      (do (aset children index new-node)
          (dom/replaceNode (aget new-node "real-node") real-node))
      (do (.push children new-node)
          (dom/appendChild (aget parent "real-node")
                           (aget new-node "real-node"))))
    (set! *node* new-node)))

(defn reconciliate-node! [tag k static-attrs dyn-attrs]
  (cond
    ;; Tags don't match -> replace the node or create a new one
    (not (ewen.inccup.core/tag-matches? tag))
    (do (obj/extend static-attrs dyn-attrs)
        (ewen.inccup.core/replace-node!
         ;;static-attrs have been mutated and now contains all attrs
         (ewen.inccup.core/new-node tag k static-attrs dyn-attrs)))
    ;; Tags match -> reconciliate attributes
    :else (ewen.inccup.core/reconciliate-attrs! dyn-attrs)))

;; tag= key= -> comp attrs
;; if key
;;    find key
;;      if tag!= -> error else -> comp attrs
;; else if tag!= create node
;; else comp attrs

(defn patch! [root vtree new-vtree]
  (binding [*node* #js {:real-node root
                        :children vtree}
            *previous-node* nil]
    (new-vtree)
    (aget *node* "children")))

(comment
  (def ee (fn [e] e))

  (satisfies? Html (specify! ee Html))

  (def tmpl (comp/html [:div {:class "e"}]
                       [:span {:id "g"}]))
  (def tmpl-state
    (patch! (.getElementById js/document "root") #js [] tmpl))

  (def tmpl-state
    (patch! (.getElementById js/document "root") tmpl-state tmpl))
  )
