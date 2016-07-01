(ns ewen.inccup.incremental.vdom
  (:require [clojure.string :as str]
            [ewen.inccup.common.util :as util]
            [ewen.inccup.common.runtime :as c-runtime]
            [goog.object]
            [goog.dom]))

(def ^:dynamic *globals* nil)

(deftype Component [id static ^:mutable params
                    var-deps forms count-dynamic])

;; Dynamic virtual nodes / tags / attributes are reified into their
;; corresponding deftype, partly because it make dispatching on their type
;; a bit easier to read, partly because it let us the possibility to
;; refactor the code using polymorphism later and partly because it let us
;; store metadata into the Vnode object properties such as the real dom
;; node associated with the virtual node, its parent, the virtual node
;; key ...
;; In particular, TextVnode would otherwise have been modeled as strings,
;; but strings can't be attached properties.
;; Dynamic tags are splitted between dynamic tags with static attributes
;; and dynamic tags with dynamic attributes. The reason is when a tag is
;; changed, its real dom node must be recreated as well as its attributes.
;; When the attributes are dynamic, special care must be taken because the
;; attributes may not have changed, attributes may have changed at the
;; same time than the tag or the attributes may even not be attributes
;; anymore but may now represent the first child of the node.

(deftype TextVnode [^:mutable text])
(deftype TagStaticAttrsVnode [^:mutable tag attrs])
(deftype TagDynamicAttrsVnode [^:mutable tag])
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
                          (instance? SeqVnode x) (.-vnodes x)
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
        length (or end-index (.-length vnodes))
        parent (oget vseq "inccup/parent-node")]
    (loop [index (dec length)]
      (when (>= index start-index)
        (let [element (aget vnodes index)]
          (when (instance? Component element)
            (remove-comp element removed-keys)
            (when-let [key (oget element "inccup/key")]
              (goog.object/remove (oget vseq "inccup/local-keymap") key)))
          (when element
            (.removeChild parent (oget element "inccup/node"))
            (.pop vnodes)))
        (recur (dec index))))))

(defn get-ref-node [prev-element]
  (cond (instance? Component prev-element)
        (or (goog.object/get
             prev-element "inccup/placeholder")
            (goog.object/get
             prev-element "inccup/node"))
        (instance? SeqVnode prev-element)
        (-> (.-vnodes prev-element)
            (aget 0)
            (oget "inccup/node"))
        :else
        (oget prev-element "inccup/node")))

(defn remove-prev-element
  [element prev-forms index removed-keys]
  (let [prev-element (aget prev-forms index)]
    (cond (instance? Component prev-element)
          (let [prev-node (or (goog.object/get
                               prev-element "inccup/placeholder")
                              (goog.object/get
                               prev-element "inccup/node"))]
            (remove-comp prev-element removed-keys)
            (goog.dom/removeNode prev-node))
          (instance? SeqVnode prev-element)
          (pop-vseq-from-to prev-element 0 nil removed-keys)
          :else
          (goog.dom/removeNode (oget prev-element "inccup/node")))
    (aset prev-forms index element)))

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
            (oset moved-comp "inccup/parent-node" parent)
            (.insertBefore parent (oget moved-comp "inccup/node") ref-node))
          (do
            (create-comp* parent ref-node element keymap removed-keys)
            (when key
              (oset (oget vseq "inccup/local-keymap") key index)))))
      ;; Text vnode
      (let [new-node (.createTextNode js/document (.-text element))]
        (oset element "inccup/node" new-node)
        (oset element "inccup/parent-node" parent)
        (.insertBefore parent new-node ref-node)))))

(defn create-dynamic
  [element prev-forms index keymap removed-keys]
  (let [parent (oget element "inccup/parent-node")
        ref-node (oget element "inccup/ref-node")]
    (cond
      (instance? Component element)
      (let [key (oget element "inccup/key")
            moved-comp (when key
                         (get-comp-with-key key keymap removed-keys))]
        (if moved-comp
          (let [node (oget moved-comp "inccup/node")]
            (.insertBefore parent node ref-node)
            (oset moved-comp "inccup/parent-node" parent)
            (aset prev-forms index moved-comp))
          (create-comp* parent ref-node element keymap removed-keys)))
      (instance? SeqVnode element)
      (let [vnodes (.-vnodes element)
            length (.-length vnodes)]
        (loop [i 0]
          (when (< i length)
            (create-dynamic-in-seq parent ref-node element vnodes i
                                   keymap removed-keys)
            (recur (inc i)))))
      ;;TextVnode
      :else
      (let [new-node (.createTextNode js/document (.-text element))]
        (oset element "inccup/node" new-node)
        (.insertBefore parent new-node ref-node)))))

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
        new-start-key (volatile! (oget @new-start-vnode "inccup/key"))
        prev-end-key (volatile! (oget @prev-end-vnode "inccup/key"))
        new-end-key (volatile! (oget @new-end-vnode "inccup/key"))
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
                    (oset moved-comp "inccup/parent-node" parent)
                    (.insertBefore parent
                                   (oget moved-comp "inccup/node")
                                   (oget @prev-start-vnode "inccup/node")))
                  (if (instance? Component @new-start-vnode)
                    (let [ref-node (oget @prev-start-vnode "inccup/node")]
                      (create-comp* parent ref-node
                                    @new-start-vnode
                                    keymap removed-keys))
                    (let [new-node (.createTextNode
                                    js/document (.-text @new-start-vnode))]
                      (oset @new-start-vnode "inccup/node" new-node)
                      (oset @new-start-vnode "inccup/parent-node" parent)
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

(defn diff-dynamic-default
  [parent element prev-element prev-forms index removed-keys]
  (let [text-vnode (if (instance? TextVnode element)
                     element (->TextVnode (str element)))
        new-node (.createTextNode js/document (.-text text-vnode))
        ref-node (get-ref-node prev-element)]
    (oset text-vnode "inccup/node" new-node)
    (oset text-vnode "inccup/parent-node" parent)
    (.insertBefore parent new-node ref-node)
    (remove-prev-element text-vnode prev-forms index removed-keys)))

(defn diff-children
  [prev-element element prev-forms index keymap removed-keys]
  (let [prev-element (aget prev-forms index)
        parent (oget prev-element "inccup/parent-node")]
    (cond
      (instance? Component element)
      (if-let [key (oget element "inccup/key")]
        (if (and (instance? Component prev-element)
                 (= key (oget prev-element "inccup/key")))
          (update-comp* prev-element element keymap removed-keys)
          (if-let [moved-comp (get-comp-with-key key keymap removed-keys)]
            (let [new-node (oget moved-comp "inccup/node")
                  ref-node (get-ref-node prev-element)]
              (.insertBefore parent new-node ref-node)
              (oset moved-comp "inccup/parent-node" parent)
              (update-comp* moved-comp element keymap removed-keys)
              (remove-prev-element moved-comp prev-forms
                                   index removed-keys))
            (let [ref-node (get-ref-node prev-element)]
              (create-comp* parent ref-node element keymap removed-keys)
              (remove-prev-element element prev-forms
                                   index removed-keys))))
        (if (and (instance? Component prev-element)
                 (= (.-id prev-element) (.-id element))
                 (not (oget prev-element "inccup/key")))
          (update-comp* prev-element element keymap removed-keys)
          (let [ref-node (get-ref-node prev-element)]
            (create-comp* parent ref-node element keymap removed-keys)
            (remove-prev-element element prev-forms
                                 index removed-keys))))
      (seq? element)
      (let [vseq (seq->vseq element)
            vnodes (.-vnodes vseq)
            length (.-length vnodes)]
        (cond
          (= 0 length)
          (diff-dynamic-default parent "" prev-element prev-forms
                                index removed-keys)
          (instance? SeqVnode prev-element)
          (let [prev-vseq prev-element
                prev-vnodes (.-vnodes prev-vseq)
                prev-length (.-length prev-vnodes)
                min-length (min prev-length length)
                local-keymap (oget prev-vseq "inccup/local-keymap")]
            (if (= 0 (goog.object/getCount local-keymap))
              (do
                (loop [i 0]
                  (when (< i min-length)
                    (let [element (aget vnodes i)
                          prev-element (aget prev-vnodes i)]
                      (diff-children prev-element element prev-vnodes i
                                     keymap removed-keys)
                      (when-let [key (oget element "inccup/key")]
                        (oset local-keymap key i))
                      (recur (inc i)))))
                (when (< min-length prev-length)
                  (pop-vseq-from-to prev-vseq min-length nil removed-keys))
                (when (< min-length length)
                  (let [ref-node (-> (aget prev-vnodes (dec min-length))
                                     (oget "inccup/node")
                                     (.-nextSibling))
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
          :else
          (let [prev-node (oget prev-element "inccup/node")]
            (oset vseq "inccup/local-keymap" #js {})
            (loop [i 0]
              (when (< i length)
                (create-dynamic-in-seq parent prev-node vseq vnodes i
                                       keymap removed-keys)
                (recur (inc i))))
            (oset vseq "inccup/parent-node" parent)
            (remove-prev-element vseq prev-forms index removed-keys))))
      (instance? TextVnode prev-element)
      (let [text (if (instance? TextVnode element)
                   (.-text element) (str element))]
        (when (not= (.-text prev-element) text)
          (set! (.-text prev-element) text)
          (oset (oget prev-element "inccup/node") "nodeValue" text)))
      :else
      ;; element is a text vnode, prev-element is of different type
      (diff-dynamic-default parent element prev-element
                            prev-forms index removed-keys))))

(defn create-comp-element-dynamic
  [parent ref-node element prev-forms index]
  (cond
    (instance? Component element)
    (do
      (oset element "inccup/parent-node" parent)
      (oset element "inccup/ref-node" ref-node)
      (aset prev-forms index element))
    (seq? element)
    (let [vseq (seq->vseq element)
          vnodes (.-vnodes vseq)
          length (.-length vnodes)]
      (if (> length 0)
        (do
          (oset vseq "inccup/parent-node" parent)
          (oset vseq "inccup/ref-node" ref-node)
          (oset vseq "inccup/local-keymap" #js {})
          (aset prev-forms index vseq))
        ;; Empty seqs are considered as empty strings,
        ;; otherwise they don't get any real node attached
        (let [text-vnode (->TextVnode "")]
          (oset text-vnode "inccup/parent-node" parent)
          (oset text-vnode "inccup/ref-node" ref-node)
          (aset prev-forms index text-vnode))))
    :else (let [text-vnode (->TextVnode (str element))]
            (oset text-vnode "inccup/parent-node" parent)
            (oset text-vnode "inccup/ref-node" ref-node)
            (aset prev-forms index text-vnode)))
  ref-node)

(defn create-comp-elements
  "Walk the static tree of a component. Create dom nodes for static nodes
  and nodes with a dynamic tag but not dynamic children. All dynamic
  elements: dynamic tags, dynamic attrs and dynamic children are converted
  to their corresponding Vnode type. All Vnodes are also attached with
  their next real dom node and parent dom node. This is in order to be able
  to create the real dom nodes of the Vnodes later, using insertBefore.
  Real dom nodes of Vnodes are created later (after this function has
  returned) in order to clear the call stack after the (recursive) walk of
  the static tree. Clearing the call stack may not be necessary but I
  simply don't want to test its necessity or not. This way I don't have to
  care.
  The list of children of every node is in reverse order. This allows us to
  create real dom nodes before walking previous nodes, and thus being able
  to keep track of the next real dom node of every Vnode. If, otherwise, we
  would have walked the children in order, then it would have been easier
  to keep track of the previous reald dom node of each Vnode, but the DOM
  API does not have an insertAfter method.
  Created dom nodes are attached to the live dom as we go. We could have
  just returned the created dom node and let the calling function attach
  the node to the live DOM, but I read this way of doing is faster,
  although I did not benchmark it. Thus it may change in the future."
  [parent ref-node static forms]
  (let [maybe-tag (first static)
        maybe-attrs (second static)
        tag (if (number? maybe-tag)
              (->> maybe-tag (aget forms) name)
              maybe-tag)
        attrs (if (number? maybe-attrs)
                (->> maybe-attrs (aget forms) attrs->js)
                maybe-attrs)]
    (let [vtag (cond (and (number? maybe-tag) (number? maybe-attrs))
                     (->TagDynamicAttrsVnode tag)
                     (number? maybe-tag)
                     (->TagStaticAttrsVnode tag attrs)
                     :else tag)
          new-node (create-dom tag attrs nil)
          l (count static)
          ;; When a tag is dynamic, we keep track of its dynamic children
          ;; (including children whith a dynamic tag, but excluding
          ;; children whith a static tag and dynamic attrs) in order to be
          ;; able to update the parent real dom node of child virtual nodes
          ;; when the dynamic tag is changed. Children with dynamic
          ;; attributes only are not kept track of because we don't need
          ;; the parent reald dom node to update the attributes
          dynamic-children #js []]
      (.insertBefore parent new-node ref-node)
      (when (number? maybe-attrs)
        (let [vattrs (->AttrsVnode attrs)]
          (oset vattrs "inccup/node" new-node)
          ;; We don't need to set the parent real dom node for vattrs
          (aset forms maybe-attrs vattrs)))
      (loop [index 2
             ref-node nil]
        (when (< index l)
          (let [child (aget static index)
                ref-node (cond
                           (nil? child) ref-node
                           (number? child)
                           (do
                             (.push dynamic-children child)
                             (create-comp-element-dynamic
                              new-node ref-node (aget forms child)
                              forms child))
                           (string? child)
                           (.insertBefore
                            new-node
                            (.createTextNode js/document child)
                            ref-node)
                           :else
                           (do
                             (when (number? (aget child 0))
                               (.push dynamic-children (aget child 0)))
                             (create-comp-elements
                              new-node ref-node child forms)))]
            (recur (inc index) ref-node))))
      (when (number? maybe-tag)
        (oset vtag "inccup/node" new-node)
        (oset vtag "inccup/parent-node" parent)
        (oset vtag "inccup/dynamic-children" dynamic-children)
        (aset forms maybe-tag vtag))
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

(defn make-forms-arr [forms-fn count-dynamic]
  (let [arr #js []]
    (loop [index 0]
      (when (< index count-dynamic)
        (.push arr (forms-fn index))
        (recur (inc index))))
    arr))

(defn create-comp* [parent ref-node comp keymap removed-keys]
  (let [id (.-id comp)
        ;; I don't know why but static need a dollar to work. Static is
        ;; probably a reserved property name or something
        static (.-static$ comp)
        key (oget comp "inccup/key")
        forms (make-forms-arr (.-forms comp) (.-count_dynamic comp))]
    (maybe-set-global id
                      "static" static
                      "var-deps" (.-var-deps comp))
    (oset comp "inccup/forms" forms)
    (oset comp "inccup/var-deps-arr" #js [])
    (when key
      (goog.object/set keymap key comp))
    (swap-count-global id inc)
    (let [new-node (create-comp-elements parent ref-node static forms)]
      (oset comp "inccup/node" new-node)
      (oset comp "inccup/parent-node" parent)
      (goog.array/forEach
       forms
       (fn [dyn-element index]
         ;; Nodes with a dynamic tag and/or dynamic attributes already have
         ;; been created during the walk of the static tree
         (when (and (not (instance? TagStaticAttrsVnode dyn-element))
                    (not (instance? TagDynamicAttrsVnode dyn-element))
                    (not (instance? AttrsVnode dyn-element)))
           (create-dynamic dyn-element forms index
                           keymap removed-keys)))))))

(defn update-comp* [prev-comp comp keymap removed-keys]
  (let [params (.-params comp)
        prev-params (.-params prev-comp)
        prev-forms (oget prev-comp "inccup/forms")
        var-deps-arr (oget prev-comp "inccup/var-deps-arr")
        var-deps-arr (->> (.-var-deps comp)
                          (make-var-deps-arr
                           var-deps-arr params prev-params))
        forms-fn (.-forms comp)]
    (set! (.-params prev-comp) params)
    (goog.array/forEach
     prev-forms
     (fn [prev-element index]
       (when (aget var-deps-arr index)
         (let [dyn-element (forms-fn index)]
           (cond (or (instance? TagStaticAttrsVnode prev-element)
                     (instance? TagDynamicAttrsVnode prev-element))
                 (let [tag (name dyn-element)]
                   (when (not= (.-tag prev-element) tag)
                     (let [prev-attrs (if (instance? TagStaticAttrsVnode
                                                     prev-element)
                                        prev-element
                                        (aget prev-forms (inc index)))
                           attrs (.-attrs prev-attrs)
                           prev-node (oget prev-element "inccup/node")
                           new-node (create-dom
                                     tag attrs (.-childNodes prev-node))
                           dynamic-children
                           (oget prev-element "inccup/dynamic-children")]
                       (set! (.-tag prev-element) tag)
                       (oset prev-element "inccup/node" new-node)
                       (when (instance? TagDynamicAttrsVnode prev-element)
                         (oset prev-attrs "inccup/node" new-node))
                       ;; Update the parent node of all dynamic children
                       ;; of this element
                       (goog.array/forEach
                        dynamic-children
                        (fn [x]
                          (oset (aget prev-forms x)
                                "inccup/parent-node" new-node)))
                       ;; prev-node is the top level node of the comp.
                       ;; Its tag can be dynamic like any other tag which
                       ;; means the node may have changed. In such a case,
                       ;; we update the component node to the new one
                       (when (identical?
                              prev-node (oget prev-comp "inccup/node"))
                         (oset prev-comp "inccup/node" new-node))
                       (.replaceChild (oget prev-element
                                            "inccup/parent-node")
                                      new-node prev-node))))
                 (instance? AttrsVnode prev-element)
                 (let [new-attrs (-> (forms-fn index) attrs->js)]
                   (diff-attrs (oget prev-element "inccup/node")
                               (.-attrs prev-element)
                               new-attrs)
                   (set! (.-attrs prev-element) new-attrs))
                 :else
                 (diff-children prev-element dyn-element prev-forms index
                                keymap removed-keys))))))))

(defn render! [node comp-fn & params]
  (goog.dom/removeChildren node)
  (binding [*globals* #js {:keymap #js {}}]
    (let [comp (apply comp-fn params)]
      (create-comp* node nil comp (goog.object/get *globals* "keymap") nil)
      (oset comp "inccup/globals" *globals*)
      (oset comp "inccup/comp-fn" comp-fn)
      comp)))

(defn update! [prev-comp & params]
  (binding [*globals* (oget prev-comp "inccup/globals")]
    (assert (not (nil? *globals*)))
    (let [comp-fn (oget prev-comp "inccup/comp-fn")
          _ (assert (fn? comp-fn))
          comp (apply comp-fn params)
          keymap (goog.object/get *globals* "keymap")
          removed-keys #js {}]
      (assert (= (.-id comp) (.-id prev-comp)))
      (update-comp* prev-comp comp keymap removed-keys)
      (clean-removed-keys keymap removed-keys)
      #_(.log js/console keymap)
      #_(.log js/console removed-keys)
      prev-comp)))
