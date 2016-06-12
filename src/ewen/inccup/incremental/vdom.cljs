(ns ewen.inccup.incremental.vdom
  (:require [clojure.string :as str]
            [ewen.inccup.common.util :as util]
            [ewen.inccup.common.runtime :as c-runtime]
            [goog.object]
            [goog.dom]))

(def ^:dynamic *globals* nil)

(defn oset [o k v]
  (goog.object/set o k v)
  v)

(defn oget
  ([o k]
   (goog.object/get o k nil))
  ([o k v]
   (goog.object/get o k (or v nil))))

(defn merge-opts [prev-opts new-opts]
  (when (goog.object/containsKey new-opts "key")
    (oset prev-opts "key" (oget new-opts "key")))
  (when (goog.object/containsKey new-opts "level")
    (oset prev-opts "level" (oget new-opts "level"))))

(defn set-opts [o new-opts]
  (if-let [opts (oget o "inccup/opts")]
    (let [new-o (goog.object/clone o)]
      (merge-opts (oget new-o "inccup/opts") new-opts)
      new-o)
    (do (oset o "inccup/opts" new-opts)
        o)))

#_(defn array-with-path [path arr]
  (oset arr "inccup/update-paths" path)
  (let [current-next (aget arr "inccup/next")
        prev-arr (volatile! nil)]
    (goog.array/forEach
     arr (fn [item index arr]
           (when (array? item)
             (when @prev-arr
               (goog.object/set @prev-arr "inccup/next" item))
             (vreset! prev-arr item))))
    (when @prev-arr
      (goog.object/set @prev-arr "inccup/next" current-next)))
  arr)

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

(defn aget-in [arr path count index]
  (if (< index count)
    (recur (aget arr (aget path index)) path count (inc index))
    arr))

(declare Component)

(declare walk-children-comps)

#_(defn init-keymap [comp]
  (loop [keymap (oset comp "inccup/keymap" (js-obj))
         removed-keys (oset comp "inccup/removed-keys" (js-obj))
         i 1]
    (when (<= i max-level)
      (oset keymap i (js-obj))
      (oset removed-keys i (js-obj))
      (recur keymap removed-keys (inc i)))))

#_(defn update-parent-comps [parent-comps comp max-level]
  (if (nil? (aget parent-comps 0))
    (do
      (aset parent-comps 0 (doto (make-array max-level) (aset 0 comp)))
      (aset parent-comps 1 0))
    (let [parents (aget parent-comps 0)
          new-index (inc (aget parent-comps 1))
          new-index (if (>= new-index (count parents)) 0 new-index)]
      (aset parents new-index comp)
      (aset parent-comps 1 new-index))))

#_(defn parent-comp [parent-comps level]
  (let [parents (aget parent-comps 0)
        index (aget parent-comps 1)
        index (- index (dec level))
        index (if (< index 0) (+ index (count parents)) index)]
    (aget parents index)))

(defn walk-parent-comps [parent-comp level f key comp]
  (loop [level level
         parent-comp parent-comp]
    (cond (nil? parent-comp)
          nil
          (> level 1)
          (recur (dec level) (oget parent-comp "inccup/parent-comp"))
          :else (f parent-comp key comp))))

(defn get-comp-by-key [parent-comp key comp]
  (-> (oget parent-comp "inccup/keymap")
      (oget key)))

(defn register-comp-key [parent-comp key comp]
  (oset (oget parent-comp "inccup/keymap") key comp))

(defn remove-comp-key [parent-comp key comp]
  (oset (oget parent-comp "inccup/removed-keys") key comp))

(defn update-key-on-move [parent-comp key comp]
  (goog.object/remove (oget parent-comp "inccup/removed-keys") key))

(defn clean-comp-keys [comp]
  (let [keymap (oget comp "inccup/keymap")
        removed-keys (oget comp "inccup/removed-keys")]
    (goog.object/forEach
     removed-keys (fn [_ k _]
                    (goog.object/remove removed-keys k)
                    (goog.object/remove keymap k)))))

(defn inccup-seq? [x]
  (and (array? x) (oget x "inccup/seq")))

#_(defn pop-inccup-seq-from
  [x index parent-comps remove-element unmount-comp]
  (loop [index index
         l (count x)]
    (when (< index l)
      (let [prev-form (aget x index)]
        (cond (instance? Component prev-form)
              (do
                (when (.-key prev-form)
                    (remove-comp-key parent-comps prev-form))
                (walk-children-comps
                 prev-form unmount-comp "bottom-up")
                (unmount-comp x index prev-form))
              (inccup-seq? prev-form)
              (pop-inccup-seq-from prev-form 0 parent-comps
                                   remove-element unmount-comp)
              (nil? prev-form)
              nil
              :else
              (remove-element x index prev-form)))
      (.pop x)
      (recur (inc index) l))))

#_(declare diff-children)

#_(defn create-inccup-seq [element index form parent-comps
                         update-tag update-attribute
                         remove-element create-element move-comp
                         will-update did-update
                         mount-comp unmount-comp]
  (aset element index
        (doto #js []
          (oset "inccup/seq" true)
          (oset "inccup/seq-parent" element)
          (oset "inccup/seq-index" index)))
  (loop [inccup-seq (aget element index)
         form form
         index 0]
    (when-let [f (first form)]
      (diff-children inccup-seq index f parent-comps
                     update-tag update-attribute
                     remove-element create-element move-comp
                     will-update did-update
                     mount-comp unmount-comp)
      (recur inccup-seq (rest form) (inc index)))))

#_(defn walk-seq-comps [inccup-seq comp-fn direction]
  (loop [inccup-seq inccup-seq
         l (count inccup-seq)
         seq-index 0]
    (when (< seq-index l)
      (let [element (aget inccup-seq seq-index)]
        (cond (instance? Component element)
              (if (= "top-bottom" direction)
                (do (comp-fn inccup-seq seq-index element)
                    (walk-children-comps element comp-fn direction))
                (do (walk-children-comps element comp-fn direction)
                    (comp-fn inccup-seq seq-index element)))
              (inccup-seq? element)
              (walk-seq-comps element comp-fn direction)
              :else
              (recur inccup-seq l (inc seq-index)))))))

#_(defn walk-children-comps [comp comp-fn direction]
  (loop [parent (.-value comp)
         update-path (oget parent "inccup/update-path")
         index 0
         l (count update-path)]
    (when (< index l)
      (let [index-in-parent (aget update-path index)
            element (aget parent index-in-parent)]
        (cond (instance? Component element)
              (if (= "top-bottom" direction)
                (do (comp-fn parent index-in-parent element)
                    (walk-children-comps element comp-fn direction))
                (do (walk-children-comps element comp-fn direction)
                    (comp-fn parent index-in-parent element)))
              (inccup-seq? element)
              (walk-seq-comps element comp-fn direction)
              :else nil))
      (recur parent update-path (inc index) l))))

#_(defn delete-prev-form
  [element index prev-form parent-comps remove-element unmount-comp]
  (cond (instance? Component prev-form)
        (do
          (when (.-key prev-form)
            (remove-comp-key parent-comps prev-form))
          (walk-children-comps
           prev-form unmount-comp "bottom-up")
          (unmount-comp element index prev-form))
        (inccup-seq? prev-form)
        (pop-inccup-seq-from
         prev-form 0 parent-comps remove-element unmount-comp)
        (nil? prev-form)
        nil
        :else
        (remove-element element index)))

#_(defn diff-children [element index form parent-comps
                       update-tag update-attribute
                       remove-element create-element move-comp
                       will-update did-update
                       mount-comp unmount-comp]
    (let [prev-form (aget element index)]
      (cond (instance? Component form)
            (if-let [key (.-key form)]
              (let [level (.-level form)]
                (if (and (instance? Component prev-form)
                         (= (.-key prev-form) key)
                         (= (.-level prev-form) level))
                  (do
                    (assert (= (.-id prev-form) (.-id form)))
                    (update-comp form prev-form parent-comps
                                 update-tag update-attribute
                                 remove-element create-element
                                 move-comp
                                 will-update did-update
                                 mount-comp unmount-comp))
                  (if-let [moved-comp (->
                                       (parent-comp parent-comps level)
                                       (oget "inccup/keymap")
                                       (oget level)
                                       (oget key))]
                    (do
                      (assert (= (.-id prev-form) (.-id form)))
                      (update-key-on-move parent-comps moved-comp)
                      (move-comp element index moved-comp)
                      (update-comp form moved-comp parent-comps
                                   update-tag update-attribute
                                   remove-element create-element
                                   move-comp
                                   will-update did-update
                                   mount-comp unmount-comp)
                      (delete-prev-form element index prev-form
                                        parent-comps
                                        remove-element unmount-comp)
                      (aset element index moved-comp))
                    (do
                      (delete-prev-form element index prev-form
                                        parent-comps
                                        remove-element unmount-comp)
                      (->> (create-comp
                            form element index parent-comps
                            update-tag update-attribute
                            remove-element create-element move-comp
                            will-update did-update
                            mount-comp unmount-comp)
                           (aset element index))))))
              (if (and (instance? Component prev-form)
                       (= (.-id prev-form) (.-id form)))
                (update-comp form prev-form parent-comps
                             update-tag update-attribute
                             remove-element create-element
                             move-comp
                             will-update did-update
                             mount-comp unmount-comp)
                (do
                  (delete-prev-form element index prev-form
                                    parent-comps
                                    remove-element unmount-comp)
                  (->> (create-comp
                        form element index parent-comps
                        update-tag update-attribute
                        remove-element create-element move-comp
                        will-update did-update
                        mount-comp unmount-comp)
                       (aset element index)))))
            (seq? form)
            (if (inccup-seq? prev-form)
              (loop [form form
                     index 0]
                (if-let [f (first form)]
                  (do (diff-children prev-form index f parent-comps
                                     update-tag update-attribute
                                     remove-element create-element
                                     move-comp
                                     will-update did-update
                                     mount-comp unmount-comp)
                      (recur (rest form) (inc index)))
                  (when (< index (count prev-form))
                    (pop-inccup-seq-from
                     prev-form index parent-comps
                     remove-element unmount-comp))))
              (do
                (delete-prev-form element index prev-form
                                  parent-comps
                                  remove-element unmount-comp)
                (create-inccup-seq element index form parent-comps
                                   update-tag update-attribute
                                   remove-element create-element
                                   move-comp
                                   will-update did-update
                                   mount-comp unmount-comp)))
            (nil? form)
            (do
              (delete-prev-form element index prev-form parent-comps
                                remove-element unmount-comp)
              (aset element index nil))
            :else
            (if (and (string? prev-form) (= prev-form form))
              nil
              (do
                (delete-prev-form element index prev-form parent-comps
                                  remove-element unmount-comp)
                (create-element element index (str form) true)
                (aset element index (str form)))))))

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

(deftype Component
    [id static ^:mutable params var-deps forms count-dynamic]
  IDeref
  (-deref [_] forms))

(defn next-sibling [node]
  (when node (.-nextSibling node)))

(defn prev-sibling [parent node]
  (if node
    (.-previousSibling node)
    (.-lastChild parent)))

(defn insert-before [parent new-node node]
  (.insertBefore parent new-node node)
  (next-sibling node))

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

(defn replace-or-append [parent new-node node]
  (if node
    (.replaceChild parent new-node node)
    (.appendChild parent new-node))
  new-node)

(defn maybe-set-global [id k1 v1 k2 v2]
  (when-not (oget *globals* id)
    (oset *globals* id (js-obj "count" 0 k1 v1 k2 v2))))

(defn swap-count-global [id inc-dec]
  (when *globals*
    (when-let [comp-globals (oget *globals* id)]
      (->> (oget comp-globals "count") inc-dec
           (oset comp-globals "count")))))

(defn clean-globals []
  (when *globals*
    (goog.object/forEach
     *globals* (fn [v k o]
                 (when (= 0 (oget v "count"))
                   (goog.object/remove o k))))))

(defn make-var-deps-arr [arr params prev-params var-deps]
  (loop [index 0]
    (when-let [indexes (aget var-deps index)]
      (if (identical-params? prev-params params indexes)
        (aset arr index false)
        (aset arr index true))
      (recur (inc index))))
  arr)

#_(defn make-true-arr [size]
  (let [arr (make-array size)]
    (loop [index 0]
      (when (not= index size)
        (aset arr index true)
        (recur (inc index))))
    arr))

#_(defn delete-text-node [parent node]
  (if (and node
           (= (.-nodeType node) 8)
           (= (.-nodeValue node) "inccup/text-start"))
    (loop [node node
           end-reached false]
      (if (or (nil? node) end-reached)
        node
        (let [next-node (next-sibling node)]
          (.removeChild parent node)
          (recur next-node
                 (and (= (.-nodeType node) 8)
                      (= (.-nodeValue node) "inccup/text-end"))))))
    node))

#_(defn delete-prev-element [parent node prev-element parent-comp]
  (cond
    (instance? Component prev-element)
    (let [opts (oget prev-element "inccup/opts")
          key (oget opts "key")
          level (when key (oget opts "level"))]
      (when key (walk-parent-comps
                 parent-comp level remove-comp-key
                 key prev-element))
      (when node
        (let [next-node (next-sibling node)]
          (.removeChild parent node)
          next-node)))
    (nil? prev-element) node
    (inccup-seq? prev-element)
    (loop [node node
           index 0]
      (if-let [e (aget prev-element index)]
        (recur (delete-prev-element parent node e parent-comp)
               (inc index))
        node))
    :else ;; Text node
    (delete-text-node parent node)))

(defn clean-component [comp parent-comp]
  (let [opts (oget comp "inccup/opts")
        key (oget opts "key")
        level (when key (oget opts "level"))]
    (when key (walk-parent-comps
               parent-comp level remove-comp-key
               key comp))))

(declare maybe-clean-components)

(defn pop-inccup-seq-from [prev-forms start-index length parent-comp]
  (loop [index (dec length)]
    (when (>= index start-index)
      (maybe-clean-components (aget prev-forms index) parent-comp)
      (.pop prev-forms)
      (recur (dec index)))))

(defn pop-nodes-from [dynamic-nodes start-index length]
  (loop [index (dec length)]
    (when (>= index start-index)
      (let [prev-node (aget dynamic-nodes index)]
        (if (array? prev-node)
          (pop-nodes-from prev-node 0 (.-length prev-node))
          (goog.dom/removeNode prev-node))
        (.pop dynamic-nodes)
        (recur (dec index))))))

(defn replace-node
  [element prev-forms index dynamic-nodes new-node parent-comp]
  (let [prev-node (aget dynamic-nodes index)]
    (maybe-clean-components
     (aget prev-forms index) parent-comp)
    (aset prev-forms index element)
    (aset dynamic-nodes index new-node)
    (if (array? prev-node)
      (do
        (replace-node (aget prev-node 0) new-node)
        (pop-nodes-from prev-node 1 (.-length prev-node)))
      (goog.dom/replaceNode new-node prev-node))))

(defn remove-node [prev-node]
  (if (array? prev-node)
    (pop-nodes-from prev-node 0 (.-length prev-node))
    (goog.dom/removeNode prev-node)))

(defn parent-node [node]
  (if (array? node)
    (recur (aget node 0))
    (.-parentNode node)))

(defn maybe-clean-components [prev-element parent-comp]
  (cond
    (instance? Component prev-element)
    (clean-component prev-element parent-comp)
    (inccup-seq? prev-element)
    (pop-inccup-seq-from prev-element 0 (.-length prev-element)
                         parent-comp)))

(declare create-comp*)
(declare update-comp*)

(defn inccup-seq []
  (doto #js [] (oset "inccup/seq" true)))

#_(defn pop-inccup-seq-from [parent node i-seq dyn-nodes
                           start-index parent-comp]
  (loop [index (.-length i-seq)]
    (when (>= index start-index)
      (goog.dom/removeElement (aget dyn-nodes index))
      (.pop dyn-nodes)
      (.pop i-seq)
      (recur (dec index)))))

#_(defn create-text-node [parent next-node data]
  (.insertBefore
   parent (.createComment js/document "inccup/text-start") next-node)
  (.insertBefore
   parent (.createTextNode js/document data)
   next-node)
  (.insertBefore
   parent (.createComment js/document "inccup/text-end") next-node)
  next-node)

#_(defn skip-text-node [node]
  ;; if node is a comment node representing the start of a text node
  (if (and node
           (= (.-nodeType node) 8)
           (= (.-nodeValue node) "inccup/text-start"))
    (loop [node (.-nextSibling node)]
      (if (or (nil? node)
              (and (= (.-nodeType node) 8)
                   (= (.-nodeValue node) "inccup/text-end")))
        (next-sibling node)
        (recur (.-nextSibling node))))
    node))

#_(defn set-text-node [parent node data]
  (if (or (nil? node)
          (not= (.-nodeType node) 8)
          (not= (.-nodeValue node) "inccup/text-start"))
    (create-text-node parent node data)
    (loop [node (.-nextSibling node)
           text-set false]
      (cond (or (nil? node)
                (and (= (.-nodeType node) 8)
                     (= (.-nodeValue node) "inccup/text-end")))
            (do (when-not text-set
                  (.insertBefore
                   parent (.createTextNode js/document data) node))
                (next-sibling node))
            ;; a text node
            (and (= (.-nodeType node) 3) (not text-set))
            (do (oset node "nodeValue" data)
                (recur (.-nextSibling node) true))
            :else
            (let [next-node (.-nextSibling node)]
              (.removeChild parent node)
              (recur next-node text-set))))))

(defn create-dynamic [parent element prev-forms index
                      dynamic-nodes parent-comp]
  (cond
    (instance? Component element)
    (let [new-node (create-comp* element parent-comp)]
      (.appendChild parent new-node)
      (aset dynamic-nodes index new-node)
      (aset prev-forms index element))
    (seq? element)
    (let [inccup-seq (inccup-seq)
          nodes (make-array (count element))]
      (loop [elements element
             i 0]
        (if-not (empty? elements)
          (do
            (create-dynamic parent (first elements) inccup-seq i
                            nodes parent-comp)
            (recur (rest elements) (inc i)))
          (do
            (aset prev-forms index inccup-seq)
            (aset dynamic-nodes index nodes)))))
    :else
    (let [new-node (.createTextNode js/document (str element))]
      (aset prev-forms index (str element))
      (aset dynamic-nodes index new-node)
      (.appendChild parent new-node))))

(defn diff-children
  [prev-element element prev-forms index dynamic-nodes parent-comp]
  (cond
    (instance? Component element)
    (let [opts (oget element "inccup/opts")
          key (oget opts "key")
          level (when key (oget opts "level"))]
      (if key
        (if (and (instance? Component prev-element)
                 (= (-> (oget prev-element "inccup/opts")
                        (oget "key")) key)
                 (= (-> (oget prev-element "inccup/opts")
                        (oget "level")) level))
          (do
            (assert (= (.-id prev-element) (.-id element)))
            (update-comp* prev-element element))
          (if-let [moved-comp (walk-parent-comps
                               parent-comp level get-comp-by-key
                               key element)]
            (do
              (assert (= (.-id moved-comp) (.-id element)))
              (let [moved-node (oget moved-comp "inccup/node")]
                (replace-node moved-comp prev-forms index dynamic-nodes
                              moved-node parent-comp)
                (walk-parent-comps
                 parent-comp level update-key-on-move key element)
                (-> (oget moved-comp "inccup/opts")
                    (oset "level" level))
                (update-comp* moved-comp element)))
            (replace-node element prev-forms index dynamic-nodes
                          (create-comp* element parent-comp)
                          parent-comp)))
        (if (and (instance? Component prev-element)
                 (= (.-id prev-element) (.-id element)))
          (update-comp* prev-element element)
          (replace-node element prev-forms index dynamic-nodes
                        (create-comp* element parent-comp)
                        parent-comp))))
    (seq? element)
    (if (inccup-seq? prev-element)
      (let [nodes (aget dynamic-nodes index)
            prev-length (.-length prev-element)
            length (count element)
            min-length (min prev-length length)
            i (volatile! 0)
            rest-elements
            (loop [elements element]
              (if (< @i min-length)
                (do
                  (diff-children (aget prev-element @i) (first elements)
                                 prev-element @i nodes parent-comp)
                  (vswap! i inc)
                  (recur (rest elements)))
                elements))]
        (when (< min-length prev-length)
          (pop-inccup-seq-from prev-element min-length
                               prev-length parent-comp)
          (pop-nodes-from nodes min-length prev-length))
        (when (< min-length length)
          (let [parent (parent-node (aget nodes (dec @i)))]
            (loop [elements rest-elements]
              (when (and (< @i length) parent)
                (create-dynamic parent (first elements) prev-element
                                @i nodes parent-comp)
                (vswap! i inc)
                (recur (rest elements)))))))
      (let [prev-node (aget dynamic-nodes index)
            parent (parent-node prev-node)]
        (remove-node prev-node)
        (maybe-clean-components (aget prev-forms index) parent-comp)
        (when parent
          (create-dynamic parent element prev-forms index
                          dynamic-nodes parent-comp))))
    (or (string? prev-element) (nil? prev-element))
    (when (not= prev-element (str element))
      (aset prev-forms index (str element))
      (oset (aget dynamic-nodes index) "nodeValue" (str element)))
    :else
    (replace-node element prev-forms index dynamic-nodes
                  (.createTextNode js/document (str element))
                  parent-comp)))

(defn create-comp-elements
  "Walk the static tree of a component. Creates dom nodes during the walk.
   Returns the created node"
  [static forms forms-fn dynamic-nodes parent-comp]
  (let [maybe-tag (first static)
        maybe-attrs (second static)
        tag (if (number? maybe-tag)
              (->> maybe-tag forms-fn name (aset forms maybe-tag))
              maybe-tag)
        attrs (if (number? maybe-attrs)
                (->> maybe-attrs forms-fn attrs->js
                     (aset forms maybe-attrs))
                maybe-attrs)]
    (let [new-node (create-dom tag attrs nil)
          l (count static)]
      (when (number? maybe-tag)
        (aset dynamic-nodes maybe-tag new-node))
      (when (number? maybe-attrs)
        (aset dynamic-nodes maybe-attrs new-node))
      (loop [index 2]
        (when (< index l)
          (let [child (aget static index)]
            (cond
              (nil? child)
              nil
              (number? child)
              (create-dynamic new-node (forms-fn child) forms child
                              dynamic-nodes parent-comp)
              (string? child)
              (create-dynamic new-node child forms child
                              dynamic-nodes parent-comp)
              :else
              (->> (create-comp-elements child forms forms-fn
                                         dynamic-nodes parent-comp)
                   (.appendChild new-node))))
          (recur (inc index))))
      new-node)))

#_(defn keep-walking-path? [update-path var-deps-arr]
  (let [l (count update-path)]
    (loop [index 0]
      (if (< index l)
        (if (aget var-deps-arr (aget update-path index))
          true
          (recur (inc index)))
        false))))

#_(defn walk-inccup-seq [node x]
  (let [l (count x)]
    (loop [node node
           index 0]
      (if (< index l)
        (let [e (aget x index)]
          (cond (nil? e) (recur node (inc index))
                (inccup-seq? e) (recur (walk-inccup-seq node e)
                                       (inc index))
                :else (recur (next-sibling node) (inc index))))
        node))))

#_(defn diff-attrs [node prev-attrs attrs]
  (loop [attrs-keys (keys attrs)]
    (when-let [k (first attrs-keys)]
      (let [v (str (get attrs k))
            prev-v (str (get prev-attrs k))]
        (when (not= v prev-v)
          (.setAttribute node (name k) v))
        (recur (rest attrs-keys)))))
  (loop [attrs-keys (keys prev-attrs)]
    (when-let [k (first attrs-keys)]
      (when-not (get attrs k)
        (.removeAttribute node (name k)))
      (recur (rest attrs-keys)))))

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
  [static var-deps-arr prev-forms forms-fn dynamic-nodes parent-comp]
  (cond
    (nil? static)
    nil
    (string? static)
    nil
    (number? static)
    ;; We just encountered a dynamic child. Diff it against the previous
    ;; child if the params it depends on did change.
    (when (aget var-deps-arr static)
      (diff-children (aget prev-forms static) (forms-fn static)
                     prev-forms static dynamic-nodes parent-comp))
    :else
    (let [tag (first static)
          dynamic-tag? (number? tag)
          prev-tag (if dynamic-tag? (aget prev-forms tag) tag)
          new-tag (if (and dynamic-tag? (aget var-deps-arr tag))
                    (name (forms-fn tag))
                    prev-tag)
          attrs (second static)
          prev-attrs (if (number? attrs)
                       (aget prev-forms attrs)
                       attrs)
          new-attrs (if (and (number? attrs) (aget var-deps-arr attrs))
                      (attrs->js (forms-fn attrs))
                      prev-attrs)
          ;; If the tag did change, replace the current node by a
          ;; node of a new type and move the children of the old node
          ;; to the new one.
          maybe-new-node
          (when (not= prev-tag new-tag)
            (let [old-node (aget dynamic-nodes tag)
                  new-node (create-dom new-tag new-attrs
                                       (.-childNodes old-node))]
              (aset dynamic-nodes tag new-node)
              (aset prev-forms tag new-tag)
              (goog.dom/replaceNode new-node old-node)
              new-node))
          l (count static)]
      ;; Update the node attributes if the params it depends on
      ;; did change and if the node tag did not change
      (when (and (= prev-tag new-tag)
                 (not (identical? prev-attrs new-attrs)))
        (diff-attrs (aget dynamic-nodes attrs) prev-attrs new-attrs))
      (when (not (identical? prev-attrs new-attrs))
        (aset prev-forms attrs new-attrs))
      (loop [index 2]
        (when (< index l)
          (update-comp-elements (aget static index) var-deps-arr
                                prev-forms forms-fn dynamic-nodes
                                parent-comp)
          (recur (inc index))))
      maybe-new-node)))

(defn create-comp* [comp parent-comp]
  (maybe-set-global (.-id comp)
                    "static" (.-static$ comp)
                    "var-deps" (.-var-deps comp))
  (let [opts (oget comp "inccup/opts")
        key (oget opts "key")
        level (when key (oget opts "level"))
        forms-fn (.-forms comp)
        count-dynamic (.-count_dynamic comp)
        forms (-> count-dynamic make-array)
        var-deps-arr (-> count-dynamic make-array)
        dynamic-nodes (-> count-dynamic make-array)
        parent-comps []]
    (oset comp "inccup/forms" forms)
    (oset comp "inccup/var-deps-arr" var-deps-arr)
    (oset comp "inccup/dynamic-nodes" dynamic-nodes)
    (oset comp "inccup/parent-comp" parent-comp)
    (oset comp "inccup/keymap" (js-obj))
    (oset comp "inccup/removed-keys" (js-obj))
    (when key
      (walk-parent-comps
       parent-comp level register-comp-key
       key comp))
    (swap-count-global (.-id comp) inc)
    (let [new-node (create-comp-elements
                    (.-static$ comp) forms forms-fn dynamic-nodes comp)]
      (oset comp "inccup/node" new-node)
      new-node)))

(defn update-comp* [prev-comp comp]
  (let [params (.-params comp)
        prev-params (.-params prev-comp)
        prev-forms (oget prev-comp "inccup/forms")
        var-deps-arr (oget prev-comp "inccup/var-deps-arr")
        var-deps-arr (->> (.-var-deps comp)
                          (make-var-deps-arr
                           var-deps-arr params prev-params))
        dynamic-nodes (oget prev-comp "inccup/dynamic-nodes")
        forms-fn (.-forms comp)
        maybe-new-node (update-comp-elements (.-static$ comp) var-deps-arr
                                             prev-forms forms-fn
                                             dynamic-nodes prev-comp)]
    (set! (.-params prev-comp) params)
    (when maybe-new-node
      (oset prev-comp "inccup/node" maybe-new-node))
    (clean-comp-keys prev-comp)))

(defn render! [node comp-fn & params]
  (binding [*globals* #js {}]
    (let [comp (apply comp-fn params)
          new-node (create-comp* comp nil)]
      (oset comp "inccup/globals" *globals*)
      (oset comp "inccup/node" new-node)
      (goog.dom/removeChildren node)
      (.appendChild node new-node)
      comp)))

(defn update! [prev-comp comp-fn & params]
  (binding [*globals* (oget prev-comp "inccup/globals")]
    (assert (not (nil? *globals*)))
    (let [comp (apply comp-fn params)]
      (assert (= (.-id comp) (.-id prev-comp)))
      (update-comp* prev-comp comp)
      (clean-globals)
      prev-comp)))
