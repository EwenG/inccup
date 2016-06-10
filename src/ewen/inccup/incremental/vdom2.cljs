(ns ewen.inccup.incremental.vdom2
  (:require [ewen.inccup.incremental.vdom :as vdom
             :refer [oset oget *globals* keep-walking-path?
                     make-true-arr attrs->js attr-as-prop]]
            [ewen.inccup.incremental.component :refer [Component]]
            [ewen.inccup.common.util :as util]
            [clojure.string :as str]
            [goog.object]
            [goog.dom]))

(def ^:dynamic *parent-node* nil)
(def ^:dynamic *current-node* nil)
(def ^:dynamic *current-vnode* nil)
(def ^:dynamic *current-vnode-length* nil)
(def ^:dynamic *current-vnode-index* nil)

(def textual-context-tags
  #js {"a" true "abbr" true "b" true "bdi" true "bdo" true "br" true
       "cite" true "code" true "data" true "dfn" true "em" true "i" true
       "kbd" true "mark" true "q" true "rp" true "rt" true "rtc" true
       "ruby" true "s" true "samp" true "small" true "span" true
       "strong" true "sub" true "sup" true "time" true "u" true "var" true
       "wbr" true "del" true "ins" true "acronym" true "big" true
       "blink" true "strike" true "tt" true "xmp" true "SAMP" true
       "ABBR" true "SUP" true "S" true "BDI" true "BR" true "RTC" true
       "SUB" true "Q" true "VAR" true "INS" true "EM" true "STRIKE" true
       "SMALL" true "TT" true "ACRONYM" true "CITE" true "KBD" true
       "RT" true "DEL" true "SPAN" true "XMP" true "BDO" true "A" true
       "TIME" true "RUBY" true "I" true "B" true "BIG" true "DFN" true
       "RP" true "BLINK" true "U" true "STRONG" true "MARK" true
       "DATA" true "CODE" true "WBR" true})

(defn create-dom [tag attrs children]
  (let [tag
        ;;IE
        (if (and
             (not goog.dom.BrowserFeature.CAN_ADD_NAME_OR_TYPE_ATTRIBUTES)
             (or (oget attrs "name")
                 (oget attrs "type")))
          (let [tag-arr #js ["<" tag]]
            (when-let [attr-name (oget attrs "name")]
              (.push
               tag-arr " name=\"" (util/escape-string attr-name) "\""))
            (when-let [attr-type (oget attrs "type")]
              (.push
               tag-arr " type=\"" (util/escape-string attr-type) "\""))
            (.push tag-arr ">")
            (.join tag-arr ""))
          tag)
        element (.createElement js/document tag)]
    (goog.object/forEach
     attrs (fn [v k o]
             (if-let [prop-name (attr-as-prop k)]
               (oset element prop-name (oget attrs k))
               (.setAttribute element k (oget attrs k)))))
    (when children
      (loop []
        (when-let [child (aget children 0)]
          (.appendChild element child)
          (recur))))
    element))

#_(defn next-node! []
  (if (< *current-vnode-index* *current-vnode-length*)
    (do
      (set! *current-vnode-index* (inc *current-vnode-index*))
      (when-let [next-sibling (.-nextSibling *current-node*)]
        (set! *current-node* next-sibling)))
    (do
      (when (nil? *current-vnode-index*)
        (set! *current-vnode-index* 2))
      (while (>= *current-vnode-index* *current-vnode-length*)
        )))

  (if-let [next-sibling (.-nextSibling *current-node*)]
    (set! *current-node* next-sibling)
    nil))

#_(defn next-vnode! []
  (cond
    (and (nil? *current-vnode-index*) (> (.-length *current-vnode*) 2))
    (set! *current-vnode-index* 2)
    (or (nil? *current-vnode-index*)
        (>= *current-vnode-index* (dec *current-vnode-length*)))
    (do
      (set! *current-vnode* (aget *current-vnode* "inccup/next"))
      (set! *current-vnode-length* (if *current-vnode*
                                     (.-length *current-vnode*)
                                     nil))
      (set! *current-vnode-index* nil))
    :else (set! *current-vnode-index* (inc *current-vnode-index*))))

(defn child-vnode! [child]
  (set! *current-vnode* child)
  (set! *current-vnode-index* 2)
  (set! *current-vnode-length* (.-length child)))

(defn next-vnode! []
  (set! *current-vnode-index* (inc *current-vnode-index*)))

(defn child-node! []
  (set! *parent-node* *current-node*)
  (set! *current-node* (.-firstChild *parent-node*)))

(defn next-node! []
  (set! *current-node* (.-nextSibling *current-node*)))

(defn patch-comp [prev-forms forms]
  (let [vnode (aget *current-vnode* *current-vnode-index*)]
    (cond
      (nil? vnode) (next-vnode!)
      (number? vnode) (next-vnode!)
      (string? vnode)
      (loop []
        (if (nil? *current-node*)
          (let [new-node (.createTextNode js/document vnode)]
            (.appendChild *parent-node* new-node)
            (next-vnode!))
          (let [node-type (.-nodeType *current-node*)
                node-name (.-nameName *current-node*)]
            (cond (= 3 node-type)
                  (do (next-vnode!) (next-node!))
                  (or (goog.object/get textual-context-tags node-name)
                      (= 8 node-type))
                  (let [tmp-node *current-node*]
                    (next-node!)
                    (.removeChild *parent-node* tmp-node)
                    (recur))
                  :else
                  (let [new-node (.createTextNode js/document vnode)]
                    (.insertBefore *parent-node* new-node *current-node*)
                    (next-vnode!))))))
      :else
      (let [tag (if (number? (first vnode))
                  (->> (first vnode) (aget forms) name str/upper-case)
                  (first vnode))
            attrs (second vnode)
            new-attrs (if (number? attrs)
                        (attrs->js (aget forms attrs))
                        attrs)]
        (loop []
          (if (nil? *current-node*)
            (let [new-node (create-dom tag attrs nil)]
              (.appendChild *parent-node* new-node)
              (set! *current-node* new-node)
              (child-vnode! vnode)
              (child-node!))
            (let [node-type (.-nodeType *current-node*)
                  node-name (.-nameName *current-node*)]
              (cond (and (= 1 node-type) (= tag node-name))
                    (do (child-vnode! vnode) (child-node!))
                    (or (goog.object/get textual-context-tags node-name)
                        (= 8 node-type))
                    (let [tmp-node *current-node*]
                      (next-node!)
                      (.removeChild *parent-node* tmp-node)
                      (recur))
                    :else
                    (let [new-node (create-dom tag attrs nil)]
                      (.insertBefore *parent-node* new-node *current-node*)
                      (set! *current-node* new-node)
                      (child-vnode! vnode)
                      (child-node!))))))))))

(defn clear-nodes []
  (loop [last-child (.-lastChild *parent-node*)]
    (when (not (identical? last-child *current-node*))
      (.removeChild *parent-node* last-child)
      (recur (.-lastChild *parent-node*))))
  (when *current-node*
    (.removeChild *parent-node* *current-node*)))

(defn render! [node comp-fn & params]
  (binding [vdom/*globals* #js {}]
    (let [comp (if (instance? Component comp-fn)
                 comp-fn
                 (apply comp-fn params))
          forms ((.-forms comp))
          root (.-static$ comp)]
      (oset comp "inccup/forms" forms)
      (binding [*parent-node* node
                *current-node* (.-firstChild node)
                *current-vnode* root
                *current-vnode-length* 1
                *current-vnode-index* 0]
        (loop []
          (patch-comp forms forms)
          (cond (identical? *current-vnode* root)
                nil
                (< *current-vnode-index* *current-vnode-length*)
                (recur)
                :else
                (do (while (>= *current-vnode-index*
                               *current-vnode-length*)
                      (clear-nodes)
                      (set! *current-vnode*
                            (oget *current-vnode* "inccup/parent"))
                      (set! *current-vnode-index*
                            (oget *current-vnode* "inccup/parent-index"))
                      (set! *current-vnode-length*
                            (.-length *current-vnode*))
                      (set! *current-node* *parent-node*)
                      (set! *parent-node* (.-parentNode *parent-node*)))
                    (recur)))))
      (oset comp "inccup/globals" vdom/*globals*)
      #_(oset comp "inccup/node" new-node)
      comp)))
