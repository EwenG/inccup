(ns ewen.inccup.incremental.vdom
  (:require [clojure.string :as str]
            [ewen.inccup.common.util :as util]
            [ewen.inccup.common.runtime :as c-runtime]
            [goog.object]
            [goog.dom]))

(def ^:dynamic *globals* nil)

(deftype Component [id static ^:mutable params
                    var-deps forms count-dynamic])

(deftype TextVnode [^:mutable text])

(deftype TagVnode [^:mutable tag])

(deftype AttrsVnode [^:mutable attrs])

(deftype SeqVnode [vnodes])

(defn into-vnodes [vnodes x]
  (let [length (.-length x)]
    (loop [index 0]
      (when (< index length)
        (.push vnodes (aget x index))
        (recur (inc index))))))

(defn seq->vnodes [items]
  (let [vnodes (array)
        length (count items)]
    (loop [items items
           index 0]
      (when (< index length)
        (let [item (first items)]
          (cond
            (seq? item)
            (into-vnodes vnodes (seq->vnodes item))
            (instance? Component item)
            (.push vnodes item)
            :else
            (.push vnodes (->TextVnode (str item))))
          (recur (rest items) (inc index)))))
    vnodes))

(defn seq->vseq [items]
  (-> items seq->vnodes ->SeqVnode))

(defn oset [o k v]
  (goog.object/set o k v)
  v)

(defn oget
  ([o k]
   (goog.object/get o k nil))
  ([o k v]
   (goog.object/get o k (or v nil))))

(defn set-comp-key! [comp key]
  (goog.object/set comp "inccup/key" (str (.-id comp) key))
  comp)

(defn tree-with-parents [tree]
  (goog.array/forEach
   tree (fn [item index _]
          (when (array? item)
            (goog.object/set item "inccup/parent" tree)
            (goog.object/set item "inccup/parent-index" (inc index))
            (tree-with-parents item))))
  tree)

(comment
  (let [root (tree-with-parents #js [#js ["p" {} 3
                                          #js ["div" {} #js ["p2" {}]]
                                          "t"
                                          #js ["p3" {}]]])]
    (loop [current (aget root 0)
           index 2]
      (prn (aget current index))
      (cond
        (identical? root current)
        nil
        (>= index (.-length current))
        (recur (goog.object/get current "inccup/parent")
               (goog.object/get current "inccup/parent-index"))
        (array? (aget current index))
        (recur (aget current index) 2)
        :else
        (recur current (inc index)))))
  )

(defn inccup-seq? [x]
  (and (array? x) (oget x "inccup/seq")))

(defn attrs->js [attrs]
  (let [attrs-js (js-obj)
        attr-keys (keys attrs)]
    (loop [attr-keys attr-keys]
      (if-let [k (first attr-keys)]
        (do
          (when (keyword? k)
            (oset attrs-js (name k) (str (get attrs k))))
          (recur (rest attr-keys)))
        attrs-js))))

(defn identical-params? [prev-params params deps-indexes]
  (loop [index 0]
    (if-let [deps-index (aget deps-indexes index)]
      (if (identical? (aget prev-params deps-index)
                      (aget params deps-index))
        (recur (inc index))
        false)
      true)))

(defn attr-as-prop [attr]
  (case attr
    "class" "className"
    "for" "htmlFor"
    "checked" "checked"
    "multiple" "multiple"
    "muted" "muted"
    "selected" "selected"
    "value" "value"
    nil))

;; Same as goog.dom.createDom but set custom attributes as html attributes
;; instead of properties
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
               (oset element prop-name (-> (oget attrs k)
                                           util/escape-string))
               (.setAttribute element k (-> (oget attrs k)
                                            util/escape-string)))))
    (when children
      (loop []
        (when-let [child (aget children 0)]
          (.appendChild element child)
          (recur))))
    element))

(defn maybe-set-global [id k1 v1 k2 v2]
  (when-not (oget *globals* id)
    (oset *globals* id (js-obj "count" 0 k1 v1 k2 v2))))

(defn swap-count-global [id inc-dec]
  (when *globals*
    (when-let [comp-globals (oget *globals* id)]
      (->> (oget comp-globals "count") inc-dec
           (oset comp-globals "count")))))

(defn make-var-deps-arr [arr params prev-params var-deps]
  (loop [index 0]
    (when-let [indexes (aget var-deps index)]
      (if (identical-params? prev-params params indexes)
        (aset arr index false)
        (aset arr index true))
      (recur (inc index))))
  arr)

(defn remove-comp* [comp key removed-keys]
  (when (not key)
    (let [id (.-id comp)]
      (swap-count-global id dec)
      (when (= 0 (-> (oget *globals* id) (oget "count")))
        (goog.object/remove *globals* id))))
  (if (oget comp "inccup/placeholder")
    (oset comp "inccup/placeholder" nil)
    (when key
      (oset removed-keys key true))))

(defn walk-sub-comps [x removed-keys f]
  (let [forms-array (cond (instance? Component x)
                          (do
                            (f x (oget x "inccup/key") removed-keys)
                            (oget x "inccup/forms"))
                          (inccup-seq? x) x
                          :else nil)]
    (when forms-array
      (let [length (.-length forms-array)]
        (loop [index 0]
          (when (< index length)
            (walk-sub-comps (aget forms-array index) removed-keys f)
            (recur (inc index))))))))

(defn remove-comp [x removed-keys]
  (walk-sub-comps x removed-keys remove-comp*))

(defn unset-removed-key [comp key removed-keys]
  (goog.object/remove removed-keys key))

(defn move-comp [x removed-keys]
  (walk-sub-comps x removed-keys unset-removed-key)
  x)

(defn clean-removed-keys [keymap removed-keys]
  (goog.object/forEach
   removed-keys
   (fn [v key o]
     (let [comp (oget keymap key)]
       (let [id (.-id comp)]
         (swap-count-global id dec)
         (when (= 0 (-> (oget *globals* id) (oget "count")))
           (goog.object/remove *globals* id)))
       (goog.object/remove keymap key)))))

(defn pop-vseq-from-to
  [vseq start-index end-index removed-keys]
  (let [vnodes (.-vnodes vseq)
        length (or end-index (.-length vnodes))]
    (loop [index (dec length)]
      (when (>= index start-index)
        (let [element (aget vnodes index)]
          (when (instance? Component element)
            (remove-comp element removed-keys)
            (when-let [key (oget element "inccup/key")]
              (goog.object/remove (oget vseq "inccup/local-keymap") key)))
          (goog.dom/removeNode (oget element "inccup/node"))
          (.pop vnodes))
        (recur (dec index))))))

(defn replace-element
  [element prev-forms index new-node removed-keys]
  (let [prev-element (aget prev-forms index)]
    (cond (instance? Component prev-element)
          (let [prev-node (or (goog.object/get
                               prev-element "inccup/placeholder")
                              (goog.object/get
                               prev-element "inccup/node"))]
            (remove-comp prev-element removed-keys)
            (if new-node
              (goog.dom/replaceNode new-node prev-node)
              (goog.dom/removeNode prev-node)))
          (instance? SeqVnode prev-element)
          (let [parent-node (oget prev-element "inccup/parent-node")]
            (when new-node
              (.insertBefore parent-node new-node
                             (.-firstChild parent-node)))
            (pop-vseq-from-to prev-element 0 nil removed-keys))
          :else
          (if new-node
            (goog.dom/replaceNode
             new-node (oget prev-element "inccup/node"))
            (goog.dom/removeNode (oget prev-element "inccup/node"))))
    (aset prev-forms index element)))

(defn parent-node [node]
  (if (array? node)
    (recur (aget node 0))
    (.-parentNode node)))

(defn inccup-seq []
  (doto #js [] (oset "inccup/seq" true)))

(defn get-comp-with-key [key keymap removed-keys]
  (let [removed-comp? (oget removed-keys key)
        comp (oget keymap key)]
    (when comp
      (if removed-comp?
        (move-comp comp removed-keys)
        (let [placeholder (.createTextNode js/document "")]
          (goog.dom/replaceNode placeholder (oget comp "inccup/node"))
          (oset comp "inccup/placeholder" placeholder)
          comp)))))

(declare create-comp*)

(defn create-dynamic-in-seq
  [parent ref-node vseq vnodes index keymap removed-keys]
  (let [element (aget vnodes index)]
    (if (instance? Component element)
      (let [key (oget element "inccup/key")
            moved-comp (when key
                         (get-comp-with-key key keymap removed-keys))]
        (if moved-comp
          (do
            (aset vnodes index moved-comp)
            (oset (oget vseq "inccup/local-keymap") key index)
            (.insertBefore parent (oget moved-comp "inccup/node") ref-node))
          (let [new-node (create-comp* element keymap removed-keys)]
            (oset element "inccup/node" new-node)
            (when key
              (oset (oget vseq "inccup/local-keymap") key index))
            (.insertBefore parent new-node ref-node))))
      ;; Text vnode
      (let [new-node (.createTextNode js/document (.-text element))]
        (oset element "inccup/node" new-node)
        (.insertBefore parent new-node ref-node)))))

(defn create-dynamic
  [parent element prev-forms index keymap removed-keys]
  (cond
    (instance? Component element)
    (let [key (oget element "inccup/key")
          moved-comp (when key
                       (get-comp-with-key key keymap removed-keys))]
      (if moved-comp
        (let [node (oget moved-comp "inccup/node")]
          (.appendChild parent node)
          (aset prev-forms index moved-comp))
        (let [node (create-comp* element keymap removed-keys)]
          (.appendChild parent node)
          (oset element "inccup/node" node)
          (aset prev-forms index element))))
    (seq? element)
    (let [vseq (seq->vseq element)
          vnodes (.-vnodes vseq)
          length (.-length vnodes)]
      (oset vseq "inccup/local-keymap" #js {})
      (loop [i 0]
        (when (< i length)
          (create-dynamic-in-seq parent nil vseq vnodes i
                                 keymap removed-keys)
          (recur (inc i))))
      (oset vseq "inccup/parent-node" parent)
      (aset prev-forms index vseq))
    :else
    (let [new-node (.createTextNode js/document (str element))
          text-vnode (->TextVnode (str element))]
      (oset text-vnode "inccup/node" new-node)
      (aset prev-forms index text-vnode)
      (.appendChild parent new-node))))

(declare update-comp*)

(defn compatible-vnodes? [prev-vnode new-vnode prev-key new-key]
  (or (and
       prev-key new-key
       (= prev-key new-key))
      (and
       (nil? prev-key) (nil? new-key)
       (= (.-id prev-vnode) (.-id new-vnode)))
      (and (instance? TextVnode prev-vnode)
           (instance? TextVnode new-vnode))))

(defn update-vnode [prev-vnode new-vnode keymap removed-keys]
  (if (instance? Component prev-vnode)
    (update-comp* prev-vnode new-vnode keymap removed-keys)
    (when (not= (.-text prev-vnode) (.-text new-vnode))
      (oset (oget prev-vnode "inccup/node")
            "nodeValue" (.-text new-vnode)))))

(defn same-keys? [prev-vnode new-vnode]
  (= (oget prev-vnode "inccup/key")
     (oget new-vnode "inccup/key")))

(defn inc-or-dec-vnode [vnode index key vnodes inc-or-dec]
  (vreset! vnode (->> (vswap! index inc-or-dec) (aget vnodes)))
  (vreset! key (when @vnode (oget @vnode "inccup/key"))))

(defn diff-vseq-with-keys
  [prev-vseq prev-vnodes prev-length vseq vnodes length
   local-keymap keymap removed-keys]
  (let [prev-start-index (volatile! 0)
        new-start-index (volatile! 0)
        prev-end-index (volatile! (dec prev-length))
        new-end-index (volatile! (dec length))
        prev-start-vnode (volatile! (aget prev-vnodes 0))
        new-start-vnode (volatile! (aget vnodes 0))
        prev-end-vnode (volatile! (aget prev-vnodes @prev-end-index))
        new-end-vnode (volatile! (aget vnodes @new-end-index))
        prev-start-key (volatile! (oget @prev-start-vnode "inccup/key"))
        new-start-key (volatile! (when @new-start-vnode
                                   (oget @new-start-vnode "inccup/key")))
        prev-end-key (volatile! (oget @prev-end-vnode "inccup/key"))
        new-end-key (volatile! (when @new-end-vnode
                                 (oget @new-end-vnode "inccup/key")))
        parent (oget prev-vseq "inccup/parent-node")]
    (while (and (<= @prev-start-index @prev-end-index)
                (<= @new-start-index @new-end-index))
      (cond (nil? @prev-start-vnode)
            (inc-or-dec-vnode prev-start-vnode prev-start-index
                              prev-start-key prev-vnodes inc)
            (nil? @prev-end-vnode)
            (inc-or-dec-vnode prev-end-vnode prev-end-index
                              prev-end-key prev-vnodes dec)
            (compatible-vnodes? @prev-start-vnode @new-start-vnode
                                @prev-start-key @new-start-key)
            (do
              (update-vnode @prev-start-vnode @new-start-vnode
                            keymap removed-keys)
              (aset vnodes @new-start-index @prev-start-vnode)
              (when (and @new-start-key
                         (not= @prev-start-index @new-start-index))
                (oset local-keymap @new-start-key @new-start-index))
              (inc-or-dec-vnode prev-start-vnode prev-start-index
                                prev-start-key prev-vnodes inc)
              (inc-or-dec-vnode new-start-vnode new-start-index
                                new-start-key vnodes inc))
            (compatible-vnodes? @prev-end-vnode @new-end-vnode
                                @prev-end-key @new-end-key)
            (do
              (update-vnode @prev-end-vnode @new-end-vnode
                            keymap removed-keys)
              (aset vnodes @new-end-index @prev-end-vnode)
              (when (and @new-end-key
                         (not= @prev-end-index @new-end-index))
                (oset local-keymap @new-end-key @new-end-index))
              (inc-or-dec-vnode prev-end-vnode prev-end-index
                                prev-end-key prev-vnodes dec)
              (inc-or-dec-vnode new-end-vnode new-end-index
                                new-end-key vnodes dec))
            (and
             (not (nil? @prev-start-key)) (not (nil? @new-end-key))
             (= @prev-start-key @new-end-key))
            (do
              (update-vnode @prev-start-vnode @new-end-vnode
                            keymap removed-keys)
              (aset vnodes @new-end-index @prev-start-vnode)
              (.insertBefore parent
               (oget @prev-start-vnode "inccup/node")
               (.-nextSibling (oget @prev-end-vnode "inccup/node")))
              (oset local-keymap @new-end-key @new-end-index)
              (inc-or-dec-vnode prev-start-vnode prev-start-index
                                prev-start-key prev-vnodes inc)
              (inc-or-dec-vnode new-end-vnode new-end-index
                                new-end-key vnodes dec))
            (and
             (not (nil? @prev-end-key)) (not (nil? @new-start-key))
             (= @prev-end-key @new-start-key))
            (do
              (update-vnode @prev-end-vnode @new-start-vnode
                            keymap removed-keys)
              (aset vnodes @new-start-index @prev-end-vnode)
              (.insertBefore parent
               (oget @prev-end-vnode "inccup/node")
               (oget @prev-start-vnode "inccup/node"))
              (oset local-keymap @new-start-key @new-start-index)
              (inc-or-dec-vnode prev-end-vnode prev-end-index
                                prev-end-key prev-vnodes dec)
              (inc-or-dec-vnode new-start-vnode new-start-index
                                new-start-key vnodes inc))
            :else
            (do
              (if-let [moved-comp-index (and @new-start-key
                                             (oget local-keymap
                                                   @new-start-key))]
                (let [moved-comp (aget prev-vnodes moved-comp-index)]
                  (update-comp* moved-comp @new-start-vnode
                                keymap removed-keys)
                  (aset vnodes @new-start-index moved-comp)
                  (aset prev-vnodes moved-comp-index nil)
                  (.insertBefore parent
                                 (oget moved-comp "inccup/node")
                                 (oget @prev-start-vnode "inccup/node")))
                (if-let [moved-comp (and @new-start-key
                                         (get-comp-with-key
                                          @new-start-key
                                          keymap removed-keys))]
                  (do
                    (aset vnodes @new-start-index moved-comp)
                    (update-comp* moved-comp @new-start-vnode
                                  keymap removed-keys)
                    (.insertBefore parent
                                   (oget moved-comp "inccup/node")
                                   (oget @prev-start-vnode "inccup/node")))
                  (if (instance? Component @new-start-vnode)
                    (let [new-node (create-comp* @new-start-vnode
                                                 keymap removed-keys)]
                      (oset @new-start-vnode "inccup/node" new-node)
                      (.insertBefore
                       parent new-node
                       (oget @prev-start-vnode "inccup/node")))
                    (let [new-node (.createTextNode
                                    js/document (.-text @new-start-vnode))]
                      (oset @new-start-vnode "inccup/node" new-node)
                      (.insertBefore
                       parent new-node
                       (oget @prev-start-vnode "inccup/node"))))))
              (when @new-start-key
                (oset local-keymap @new-start-key @new-start-index))
              (inc-or-dec-vnode new-start-vnode new-start-index
                                new-start-key vnodes inc))))
    (oset vseq "inccup/parent-node" parent)
    (oset vseq "inccup/local-keymap" local-keymap)
    (cond (> @prev-start-index @prev-end-index)
          (let [ref-vnode (aget vnodes (inc @new-end-index))
                ref-node (when ref-vnode (oget ref-vnode "inccup/node"))]
            (loop [i @new-start-index]
              (when (<= i @new-end-index)
                (create-dynamic-in-seq parent ref-node vseq vnodes i
                                       keymap removed-keys)
                (recur (inc i)))))
          (> @new-start-index @new-end-index)
          (pop-vseq-from-to prev-vseq @prev-start-index
                            (inc @prev-end-index) removed-keys))))

;; Taken from
;; https://github.com/paldepind/snabbdom/blob/master/snabbdom.js# L133
#_(defn diff-vseq-with-keys
  [prev-forms new-forms dynamic-nodes key->index keymap removed-keys]
  (let [prev-start-index (volatile! 0)
        new-start-index (volatile! 0)
        prev-end-index (volatile! (dec (.-length prev-forms)))
        new-end-index (volatile! (dec (.-length new-forms)))
        prev-start-form (volatile! (aget prev-forms 0))
        new-start-form (volatile! (aget new-forms 0))
        prev-end-form (volatile! (aget prev-forms @prev-end-index))
        new-end-form (volatile! (aget new-forms @new-end-index))]
    (while (and (<= @prev-start-index @prev-end-index)
                (<= @new-start-index @new-end-index))
      (cond (nil? @prev-start-form)
            (vreset! prev-start-form (->> (vswap! prev-start-index inc)
                                          (aget prev-forms)))
            (nil? @prev-end-form)
            (vreset! prev-end-form (->> (vswap! prev-end-index dec)
                                        (aget prev-forms)))
            (compatible-vnodes? @prev-start-form @new-start-form)
            (do
              (aset new-forms @new-start-index @prev-start-form)
              (update-vnode @prev-start-form @new-start-form
                            keymap removed-keys)
              (vreset! prev-start-form (->> (vswap! prev-start-index inc)
                                            (aget prev-forms)))
              (vreset! new-start-form (->> (vswap! new-start-index inc)
                                           (aget new-forms))))
            (compatible-vnodes? @prev-end-form @new-end-form)
            (do
              (aset new-forms @new-end-index @prev-end-form)
              (update-vnode @prev-end-form @new-end-form
                            keymap removed-keys)
              (vreset! prev-end-form (->> (vswap! prev-end-index dec)
                                          (aget prev-forms)))
              (vreset! new-end-form (->> (vswap! new-end-index dec)
                                         (aget new-forms))))
            (same-keys? @prev-start-form @new-end-form)
            (do
              (aset new-forms @new-end-index @prev-start-form)
              (update-vnode @prev-start-form @new-end-form
                            keymap removed-keys)
              (goog.dom/insertSiblingBefore
               (aget dynamic-nodes @prev-start-index)
               (.-nextSibling (aget dynamic-nodes @prev-end-index)))
              (vreset! prev-start-form (->> (vswap! prev-start-index inc)
                                            (aget prev-forms)))
              (vreset! new-end-form (->> (vswap! new-end-index dec)
                                         (aget new-forms))))
            (same-keys? @prev-end-form @new-start-form)
            (do
              (aset new-forms @new-start-index @prev-end-form)
              (update-vnode @prev-end-form @new-start-form
                            keymap removed-keys)
              (goog.dom/insertSiblingBefore
               (aget dynamic-nodes @prev-end-index)
               (aget dynamic-nodes @prev-start-index))
              (vreset! prev-end-form (->> (vswap! prev-end-index dec)
                                          (aget prev-forms)))
              (vreset! new-start-form (->> (vswap! new-start-index inc)
                                           (aget new-forms))))
            :else
            (let [key (oget @new-start-form "inccup/key")]
              (if-let [moved-comp-index (and key (oget key->index key))]
                (let [moved-comp (aget prev-forms moved-comp-index)]
                  (aset new-forms @new-start-index moved-comp)
                  (update-comp* moved-comp @new-start-form
                                keymap removed-keys)
                  (aset new-forms @new-start-index moved-comp)
                  (aset prev-forms moved-comp-index nil)
                  (goog.dom/insertSiblingBefore
                   (oget moved-comp "inccup/node")
                   (aget dynamic-nodes @prev-start-index)))
                (if-let [moved-comp (get-comp-with-key
                                     key keymap removed-keys)]
                  (do
                    (aset new-forms @new-start-index moved-comp)
                    (goog.dom/insertSiblingBefore
                     (oget moved-comp "inccup/node")
                     (aget dynamic-nodes @prev-start-index)))
                  (goog.dom/insertSiblingBefore
                   (create-comp* @new-start-form keymap removed-keys)
                   (aget dynamic-nodes @prev-start-index))))
              (vreset! new-start-form (vreset! new-start-index inc)))))))

(defn diff-children
  [element prev-forms index keymap removed-keys in-seq?]
  (let [prev-element (aget prev-forms index)]
    (cond
      (instance? Component element)
      (if-let [key (oget element "inccup/key")]
        (if (and (instance? Component prev-element)
                 (= key (oget prev-element "inccup/key")))
          (update-comp* prev-element element keymap removed-keys)
          (if-let [moved-comp (get-comp-with-key key keymap removed-keys)]
            (let [new-node (oget moved-comp "inccup/node")]
              (replace-element moved-comp prev-forms index
                               new-node removed-keys)
              (update-comp* moved-comp element keymap removed-keys))
            (replace-element element prev-forms index
                             (create-comp* element keymap removed-keys)
                             removed-keys)))
        (if (and (instance? Component prev-element)
                 (= (.-id prev-element) (.-id element))
                 (not (oget prev-element "inccup/key")))
          (update-comp* prev-element element keymap removed-keys)
          (replace-element element prev-forms index
                           (create-comp* element keymap removed-keys)
                           removed-keys)))
      (and (not in-seq?) (seq? element))
      (let [vseq (seq->vseq element)
            vnodes (.-vnodes vseq)
            length (.-length vnodes)]
        (if (instance? SeqVnode prev-element)
          (let [prev-vseq prev-element
                prev-vnodes (.-vnodes prev-vseq)
                prev-length (.-length prev-vnodes)
                min-length (min prev-length length)
                local-keymap (oget prev-vseq "inccup/local-keymap")]
            (if (= 0 (goog.object/getCount local-keymap))
              (do
                (loop [i 0]
                  (when (< i min-length)
                    (let [element (aget vnodes i)]
                      (diff-children element prev-vnodes i
                                     keymap removed-keys true)
                      (when-let [key (oget element "inccup/key")]
                        (oset local-keymap key i))
                      (recur (inc i)))))
                (when (< min-length prev-length)
                  (pop-vseq-from-to prev-vseq min-length nil removed-keys))
                (when (< min-length length)
                  (let [ref-node (when (> min-length 0)
                                     (-> (aget prev-vnodes (dec min-length))
                                         (oget "inccup/node")
                                         (.-nextSibling)))
                        parent (oget prev-vseq "inccup/parent-node")]
                    (loop [i min-length]
                      (when (< i length)
                        (create-dynamic-in-seq parent ref-node prev-vseq
                                               vnodes i keymap removed-keys)
                        (.push prev-vnodes (aget vnodes i))
                        (recur (inc i)))))))
              (do
                (diff-vseq-with-keys prev-vseq prev-vnodes prev-length
                                     vseq vnodes length local-keymap
                                     keymap removed-keys)
                (aset prev-forms index vseq))))
          (let [prev-node (oget prev-element "inccup/node")
                parent (-> prev-node (.-parentNode))]
            (oset vseq "inccup/local-keymap" #js {})
            (loop [i 0]
              (when (< i length)
                (create-dynamic-in-seq parent prev-node vseq vnodes i
                                       keymap removed-keys)
                (recur (inc i))))
            (oset vseq "inccup/parent-node" parent)
            (replace-element vseq prev-forms index nil removed-keys))))
      (instance? TextVnode prev-element)
      (let [text (if (instance? TextVnode element)
                   (.-text element) (str element))]
        (when (not= (.-text prev-element) text)
          (set! (.-text prev-element) text)
          (oset (oget prev-element "inccup/node") "nodeValue" text)))
      :else
      ;; element is a text vnode, prev-element is of different type
      (let [text-vnode (if (instance? TextVnode element)
                         element (->TextVnode (str element)))
            new-node (.createTextNode js/document (.-text text-vnode))]
        (oset text-vnode "inccup/node" new-node)
        (replace-element text-vnode prev-forms index
                         new-node removed-keys)))))

(defn create-comp-elements
  "Walk the static tree of a component. Creates dom nodes during the walk.
   Returns the created node"
  [static forms forms-fn keymap comp-moves]
  (let [maybe-tag (first static)
        maybe-attrs (second static)
        tag (if (number? maybe-tag)
              (->> maybe-tag forms-fn name)
              maybe-tag)
        attrs (if (number? maybe-attrs)
                (->> maybe-attrs forms-fn attrs->js)
                maybe-attrs)]
    (let [new-node (create-dom tag attrs nil)
          l (count static)]
      (when (number? maybe-tag)
        (let [vtag (->TagVnode tag)]
          (oset vtag "inccup/node" new-node)
          (aset forms maybe-tag vtag)))
      (when (number? maybe-attrs)
        (let [vattrs (->AttrsVnode attrs)]
          (oset vattrs "inccup/node" new-node)
          (aset forms maybe-attrs vattrs)))
      (loop [index 2]
        (when (< index l)
          (let [child (aget static index)]
            (cond
              (nil? child)
              nil
              (number? child)
              (create-dynamic new-node (forms-fn child) forms
                              child keymap comp-moves)
              (string? child)
              (create-dynamic new-node child forms child
                              keymap comp-moves)
              :else
              (->> (create-comp-elements child forms forms-fn
                                         keymap comp-moves)
                   (.appendChild new-node))))
          (recur (inc index))))
      new-node)))

(defn diff-attrs [node prev-attrs attrs]
  (let [new-attrs-keys (js-obj)]
    (goog.object/forEach
     attrs (fn [v k _]
             (let [prev-v (oget prev-attrs k)]
               (oset new-attrs-keys k nil)
               (when (not= prev-v v)
                 (if-let [prop-name (attr-as-prop k)]
                   (oset node prop-name (util/escape-string v))
                   (.setAttribute node k (util/escape-string v)))
                 (oset prev-attrs k v)))))
    (goog.object/forEach
     prev-attrs (fn [v k _]
                  (when-not (goog.object/containsKey new-attrs-keys k)
                    (if-let [prop-name (attr-as-prop k)]
                      (oset node prop-name "")
                      (.removeAttribute node k))
                    (goog.object/remove prev-attrs k))))))

(defn update-comp-elements
  "Walk a component. Updates dynamic parts of the component during the
  walk. `node` is the current node beeing walked. `parent` is its parent
  node. Returns the next sibling of `node`"
  [comp comp-node static var-deps-arr prev-forms forms-fn
   keymap removed-keys]
  (cond
    (nil? static)
    nil
    (string? static)
    nil
    (number? static)
    ;; We just encountered a dynamic child. Diff it against the previous
    ;; child if the params it depends on did change.
    (when (aget var-deps-arr static)
      (diff-children (forms-fn static) prev-forms static
                     keymap removed-keys false))
    :else
    (let [tag (first static)
          dynamic-tag? (number? tag)
          prev-tag (if dynamic-tag? (aget prev-forms tag) tag)
          new-tag (if (and dynamic-tag? (aget var-deps-arr tag))
                    (-> (forms-fn tag) name ->TagVnode)
                    prev-tag)
          attrs (second static)
          dynamic-attrs? (number? attrs)
          prev-attrs (if dynamic-attrs?
                       (aget prev-forms attrs)
                       (->AttrsVnode attrs))
          new-attrs (if (and dynamic-attrs? (aget var-deps-arr attrs))
                      (-> (forms-fn attrs) attrs->js ->AttrsVnode)
                      prev-attrs)
          l (count static)]
      ;; If the tag did change, replace the current node by a
      ;; node of a new type and move the children of the old node
      ;; to the new one.
      (cond (and dynamic-tag? (not= (.-tag prev-tag) (.-tag new-tag)))
            (let [old-node (oget prev-tag "inccup/node")
                  new-node (create-dom (.-tag new-tag) (.-attrs new-attrs)
                                       (.-childNodes old-node))]
              (oset new-tag "inccup/node" new-node)
              (aset prev-forms tag new-tag)
              (when dynamic-attrs?
                (oset new-attrs "inccup/node" new-node)
                (aset prev-forms attrs new-attrs))
              (when (identical? comp-node old-node)
                (oset comp "inccup/node" new-node))
              (goog.dom/replaceNode new-node old-node))
            ;; Update the node attributes if the params it depends on
            ;; did change and if the node tag did not change
            (not (identical? (.-attrs prev-attrs) (.-attrs new-attrs)))
            (let [new-attrs (.-attrs new-attrs)]
              (diff-attrs (oget prev-attrs "inccup/node")
                          (.-attrs prev-attrs) new-attrs)
              (set! (.-attrs prev-attrs) new-attrs)))
      (loop [index 2]
        (when (< index l)
          (update-comp-elements comp comp-node (aget static index)
                                var-deps-arr prev-forms forms-fn
                                keymap removed-keys)
          (recur (inc index)))))))

(defn create-comp* [comp keymap removed-keys]
  (let [id (.-id comp)
        static (.-static$ comp)
        key (oget comp "inccup/key")
        count-dynamic (.-count_dynamic comp)
        forms (-> count-dynamic make-array)
        var-deps-arr (-> count-dynamic make-array)]
    (maybe-set-global id
                      "static" static
                      "var-deps" (.-var-deps comp))
    (oset comp "inccup/forms" forms)
    (oset comp "inccup/var-deps-arr" var-deps-arr)
    (when key
      (goog.object/set keymap key comp))
    (swap-count-global id inc)
    (let [new-node (create-comp-elements static forms (.-forms comp)
                                         keymap removed-keys)]
      (oset comp "inccup/node" new-node)
      new-node)))

(defn update-comp* [prev-comp comp keymap removed-keys]
  (let [params (.-params comp)
        prev-params (.-params prev-comp)
        prev-forms (oget prev-comp "inccup/forms")
        var-deps-arr (oget prev-comp "inccup/var-deps-arr")
        var-deps-arr (->> (.-var-deps comp)
                          (make-var-deps-arr
                           var-deps-arr params prev-params))
        forms-fn (.-forms comp)]
    (update-comp-elements prev-comp (oget prev-comp "inccup/node")
                          (.-static$ comp) var-deps-arr
                          prev-forms forms-fn
                          keymap removed-keys)
    (set! (.-params prev-comp) params)))

(defn render! [node comp-fn & params]
  (binding [*globals* #js {:keymap #js {}}]
    (let [comp (apply comp-fn params)
          new-node (create-comp*
                    comp (goog.object/get *globals* "keymap") nil)]
      (oset comp "inccup/globals" *globals*)
      (oset comp "inccup/node" new-node)
      (goog.dom/removeChildren node)
      (.appendChild node new-node)
      comp)))

(defn update! [prev-comp comp-fn & params]
  (binding [*globals* (oget prev-comp "inccup/globals")]
    (assert (not (nil? *globals*)))
    (let [comp (apply comp-fn params)
          keymap (goog.object/get *globals* "keymap")
          removed-keys #js {}]
      (assert (= (.-id comp) (.-id prev-comp)))
      (update-comp* prev-comp comp keymap removed-keys)
      (clean-removed-keys keymap removed-keys)
      #_(.log js/console keymap)
      #_(.log js/console removed-keys)
      prev-comp)))
