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

(defn array-with-path [path arr]
  (oset arr "inccup/update-paths" path)
  arr)

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
    [id static params var-deps ^:mutable forms]
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
(defn create-dom [tag attrs children void-tag?]
  (let [tag
        ;;IE
        (if (and
             (not goog.dom.BrowserFeature.CAN_ADD_NAME_OR_TYPE_ATTRIBUTES)
             (or (oget attrs "name")
                 (oget attrs "type")))
          (let [tag-arr #js ["<" tag]]
            (when-let [attr-name (oget attrs "name")]
              (.push
               tag-arr " name=\"" (goog.string/htmlEscape attr-name) "\""))
            (when-let [attr-type (oget attrs "type")]
              (.push
               tag-arr " type=\"" (goog.string/htmlEscape attr-type) "\""))
            (.push tag-arr ">")
            (.join tag-arr ""))
          tag)
        element (.createElement js/document tag)]
    (goog.object/forEach
     attrs (fn [v k o]
             (if-let [prop-name (attr-as-prop k)]
               (oset element prop-name (oget attrs k))
               (.setAttribute element k (oget attrs k)))))
    (when (and children (not void-tag?))
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
      (when (identical-params? prev-params params indexes)
        (aset arr index true))
      (recur (inc index))))
  arr)

(defn make-true-arr [size]
  (let [arr (make-array size)]
    (loop [index 0]
      (when (not= index size)
        (aset arr index true)
        (recur (inc index))))
    arr))

(defn delete-text-node [parent node]
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

(defn delete-prev-element [parent node prev-element parent-comp]
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

(declare create-comp*)
(declare update-comp*)

(defn inccup-seq []
  (doto #js [] (oset "inccup/seq" true)))

(defn pop-inccup-seq-from [parent node x index parent-comp]
  (let [l (count x)
        next-node (loop [node node
                         index index]
                    (if (< index l)
                      (recur (delete-prev-element
                              parent node (aget x index) parent-comp)
                             (inc index))
                      node))]
    (loop [index index]
      (when (< index l)
        (.pop x)
        (recur (inc index))))
    next-node))

(defn create-text-node [parent next-node data]
  (.insertBefore
   parent (.createComment js/document "inccup/text-start") next-node)
  (.insertBefore
   parent (.createTextNode js/document data) next-node)
  (.insertBefore
   parent (.createComment js/document "inccup/text-end") next-node)
  next-node)

(defn skip-text-node [node]
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

(defn set-text-node [parent node data]
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

(defn diff-children
  [parent node prev-element element prev-forms index parent-comp]
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
            (update-comp* prev-element element parent node))
          (do
            (if-let [moved-comp (walk-parent-comps
                                 parent-comp level get-comp-by-key
                                 key element)]
              (let [moved-node (oget moved-comp "inccup/node")]
                (assert (= (.-id moved-comp) (.-id element)))
                (aset prev-forms index moved-comp)
                (walk-parent-comps
                 parent-comp level update-key-on-move key element)
                (-> (oget moved-comp "inccup/opts")
                    (oset "level" level))
                (.insertBefore parent moved-node node)
                (update-comp* moved-comp element parent moved-node)
                (delete-prev-element parent node prev-element parent-comp))
              (do
                (aset prev-forms index element)
                (let [next-node  (delete-prev-element
                                  parent node prev-element parent-comp)]
                  (insert-before
                   parent (create-comp* element parent-comp) next-node))))))
        (if (and (instance? Component prev-element)
                 (= (.-id prev-element) (.-id element)))
          (update-comp* prev-element element parent node)
          (do
            (aset prev-forms index element)
            (let [next-node (delete-prev-element
                             parent node prev-element parent-comp)]
              (insert-before
               parent (create-comp* element parent-comp) next-node)
              next-node)))))
    (seq? element)
    (if (inccup-seq? prev-element)
      (loop [node node
             elements element
             i 0]
        (if-not (empty? elements)
          (let [e (first elements)
                prev-e (aget prev-element i)]
            (recur (diff-children parent node prev-e e prev-element
                                  i parent-comp)
                   (rest elements) (inc i)))
          (if (< i (count prev-element))
            (pop-inccup-seq-from parent node prev-element i parent-comp)
            node)))
      (loop [node node
             elements element
             inccup-seq (inccup-seq)
             i 0]
        (if-not (empty? elements)
          (recur (diff-children parent node nil (first elements)
                                inccup-seq i parent-comp)
                 (rest elements) inccup-seq (inc i))
          (do
            (aset prev-forms index inccup-seq)
            (delete-prev-element parent node prev-element parent-comp)))))
    (nil? element)
    (do
      (aset prev-forms index nil)
      (delete-prev-element parent node prev-element parent-comp))
    :else
    (if (string? prev-element)
      (if (= prev-element (str element))
        (skip-text-node node)
        (set-text-node parent node (str element)))
      (do
        (aset prev-forms index (str element))
        (create-text-node parent node (str element))
        (delete-prev-element parent node prev-element parent-comp)))))

(defn create-comp-elements
  "Walk the static tree of a component. Creates dom nodes during the walk.
   Returns the created node"
  [static forms parent-comp]
  (let [dynamic-tag? (number? (first static))
        tag (if dynamic-tag?
              (->> (first static) (aget forms) name
                   (aset forms (first static)))
              (first static))
        void-tag? (and dynamic-tag? (get c-runtime/void-tags tag))
        attrs (if (number? (second static))
                (->> (second static) (aget forms) attrs->js
                     (aset forms (second static)))
                (second static))]
    (let [new-node (create-dom tag attrs nil void-tag?)
          l (count static)]
      (when (not void-tag?)
        (loop [index 2]
          (when (< index l)
            (let [child (aget static index)]
              (cond
                (nil? child)
                nil
                (number? child)
                (let [form (aget forms child)]
                  (diff-children new-node nil nil form forms child
                                 parent-comp))
                (string? child)
                (diff-children new-node nil nil child forms child
                               parent-comp)
                :else
                (->> (create-comp-elements child forms parent-comp)
                     (.appendChild new-node))))
            (recur (inc index)))))
      new-node)))

(defn keep-walking-path? [update-path var-deps-arr]
  (let [l (count update-path)]
    (loop [index 0]
      (if (< index l)
        (if (aget var-deps-arr (aget update-path index))
          true
          (recur (inc index)))
        false))))

(defn walk-inccup-seq [node x]
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
                   (oset node prop-name v)
                   (.setAttribute node k v))
                 (oset prev-attrs k v)))))
    (goog.object/forEach
     prev-attrs (fn [v k _]
                  (when-not (goog.object/containsKey new-attrs-keys k)
                    (if-let [prop-name (attr-as-prop k)]
                      (oset node prop-name nil)
                      (.removeAttribute node k))
                    (goog.object/remove prev-attrs k))))))

(defn update-comp-elements
  "Walk a component. Updates dynamic parts of the component during the
  walk. `node` is the current node beeing walked. `parent` is its parent
  node. Returns the next sibling of `node`"
  [parent node static var-deps-arr prev-forms forms parent-comp]
  (cond
    (nil? static)
    node
    (number? static)
    ;; We just encountered a dynamic child. Diff it against the previous
    ;; child if the params it depends on did change.
    (let [prev-form (aget prev-forms static)]
      (if (aget var-deps-arr static)
        (diff-children parent node prev-form (aget forms static)
                       prev-forms static parent-comp)
        (cond (nil? prev-form) node
              (inccup-seq? prev-form) (walk-inccup-seq node prev-form)
              :else (next-sibling node))))
    (string? static)
    (next-sibling node)
    :else
    (let [update-paths (oget static "inccup/update-paths")]
      (if (keep-walking-path? update-paths var-deps-arr)
        (let [tag (first static)
              dynamic-tag? (number? tag)
              prev-tag (if dynamic-tag? (aget prev-forms tag) tag)
              new-tag (if (and dynamic-tag? (aget var-deps-arr tag))
                        (name (aget forms tag))
                        prev-tag)
              void-tag? (and dynamic-tag?
                             (get c-runtime/void-tags new-tag))
              attrs (second static)
              prev-attrs (if (number? attrs)
                           (aget prev-forms attrs)
                           attrs)
              new-attrs (if (and (number? attrs) (aget var-deps-arr attrs))
                          (attrs->js (aget forms attrs))
                          prev-attrs)
              ;; If the tag did change, replace the current node by a
              ;; node of a new type and move the children of the old node
              ;; to the new one.
              maybe-new-node
              (if (not= prev-tag new-tag)
                (-> (if (get c-runtime/void-tags prev-tag)
                      (create-comp-elements static forms parent-comp)
                      (create-dom new-tag new-attrs
                                  (when node
                                    (.-childNodes node))
                                  void-tag?))
                    (#(replace-or-append parent % node)))
                node)
              l (count static)]
          ;; Update the node attributes if the params it depends on
          ;; did change or if the node tag did change
          (when (or (not= prev-tag new-tag)
                    (not (identical? prev-attrs new-attrs)))
            (diff-attrs maybe-new-node prev-attrs new-attrs))
          (when (not= prev-tag new-tag)
            (aset prev-forms tag new-tag))
          (when (not (identical? prev-attrs new-attrs))
            (aset prev-forms attrs new-attrs))
          (when (not void-tag?)
            (loop [child (when maybe-new-node
                           (.-firstChild maybe-new-node))
                   index 2]
              (when (< index l)
                (recur
                 (update-comp-elements
                  maybe-new-node child (aget static index)
                  var-deps-arr prev-forms forms parent-comp)
                 (inc index)))))
          (next-sibling maybe-new-node))
        (next-sibling node)))))

(defn create-comp* [comp parent-comp]
  (maybe-set-global (.-id comp)
                    "static" (.-static$ comp)
                    "var-deps" (.-var-deps comp))
  (let [opts (oget comp "inccup/opts")
        key (oget opts "key")
        level (when key (oget opts "level"))
        var-deps-arr (-> (.-var-deps comp) count make-true-arr)
        forms ((.-forms comp) var-deps-arr)
        parent-comps []]
    (set! (.-forms comp) forms)
    (oset comp "inccup/var-deps-arr" var-deps-arr)
    (oset comp "inccup/parent-comp" parent-comp)
    (oset comp "inccup/keymap" (js-obj))
    (oset comp "inccup/removed-keys" (js-obj))
    (when key
      (walk-parent-comps
       parent-comp level register-comp-key
       key comp))
    (swap-count-global (.-id comp) inc)
    (let [new-node (create-comp-elements (.-static$ comp) forms comp)]
      (when key (oset comp "inccup/node" new-node))
      new-node)))

(defn update-comp* [prev-comp comp parent node]
  (let [params (.-params comp)
        prev-params (.-params prev-comp)
        prev-forms (.-forms prev-comp)
        var-deps-arr (oget prev-comp "inccup/var-deps-arr")
        var-deps-arr (->> (.-var-deps comp)
                          (make-var-deps-arr
                           var-deps-arr params prev-params))
        forms ((.-forms comp) var-deps-arr)
        next-node (update-comp-elements parent node (.-static$ comp)
                                        var-deps-arr prev-forms forms
                                        prev-comp)]
    (clean-comp-keys prev-comp)
    next-node))

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
    (let [comp (apply comp-fn params)
          node (oget prev-comp "inccup/node")
          parent (.-parentNode node)
          _ (assert (= (.-id comp) (.-id prev-comp)))
          next-node (update-comp* prev-comp comp parent node)]
      (oset prev-comp "inccup/node" (prev-sibling parent next-node))
      (clean-globals)
      prev-comp)))
