(ns ewen.inccup.incremental.vdom
  (:require [clojure.string :as str]
            [ewen.inccup.common.util :as util]
            [ewen.inccup.common.runtime :as c-runtime]
            [goog.object]
            [goog.dom]))

(def ^:dynamic *globals* nil)

(deftype Component
    [id static ^:mutable params var-deps forms count-dynamic]
  IDeref
  (-deref [_] forms))

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

(defn pop-seq-from
  [prev-forms dynamic-nodes start-index length removed-comps]
  (loop [index (dec length)]
    (when (>= index start-index)
      (let [element (aget prev-forms index)]
        (cond (instance? Component element)
              (.push removed-comps element)
              (inccup-seq? element)
              (pop-seq-from element (aget dynamic-nodes index)
                            0 (.-length element) removed-comps)
              :else
              (goog.dom/removeNode (aget dynamic-nodes index)))
        (.pop prev-forms)
        (.pop dynamic-nodes))
      (recur (dec index)))))

#_(defn only-first-node [node-or-nodes]
  (if (array? node-or-nodes)
    (do
      (pop-nodes-from node-or-nodes 1 (.-length node-or-nodes))
      (recur (aget node-or-nodes 0)))
    node-or-nodes))

(defn get-first-node [node-or-nodes]
  (if (array? node-or-nodes)
    (recur (aget node-or-nodes 0))
    node-or-nodes))

(defn replace-element
  [element prev-forms index dynamic-nodes new-node
   removed-comps]
  (let [prev-element (aget prev-forms index)]
    (cond (instance? Component prev-element)
          (let [prev-node (oget prev-element "inccup/node")]
            (.insertBefore (.-parentNode prev-node) new-node prev-node)
            (.push removed-comps prev-element))
          (inccup-seq? prev-element)
          (let [first-node (get-first-node dynamic-nodes)]
            (.insertBefore (.-parentNode first-node) new-node first-node)
            (pop-seq-from prev-element (aget dynamic-nodes index) 0
                          (.-length prev-element) removed-comps))
          :else
          (goog.dom/replaceNode new-node (aget dynamic-nodes index)))
    (aset prev-forms index element)
    (aset dynamic-nodes index new-node)))

(defn parent-node [node]
  (if (array? node)
    (recur (aget node 0))
    (.-parentNode node)))

(defn inccup-seq []
  (doto #js [] (oset "inccup/seq" true)))

(declare create-comp*)

(defn create-dynamic
  [parent element prev-forms index dynamic-nodes keymap]
  (cond
    (instance? Component element)
    (let [new-node (create-comp* element keymap)]
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
            (create-dynamic parent (first elements)
                            inccup-seq i nodes keymap)
            (recur (rest elements) (inc i)))
          (do
            (aset prev-forms index inccup-seq)
            (aset dynamic-nodes index nodes)))))
    :else
    (let [new-node (.createTextNode js/document (str element))]
      (aset prev-forms index (str element))
      (aset dynamic-nodes index new-node)
      (.appendChild parent new-node))))

#_(defn diff-children
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

(defn add-comp-move [prev-comp-or-node key comp-moves]
  (assert (not (goog.object/containsKey comp-moves key)))
  (goog.object/set comp-moves key prev-comp-or-node))

(defn handle-moved-comps [comp comp-node keymap comp-moves]
  (if-let [key (-> (oget comp "inccup/opts")
                   (oget "key"))]
    (let [key (str (.-id comp) key)]
      (if-let [replaced-comp-or-node (oget comp-moves key)]
        (do
          (goog.object/remove comp-moves key)
          (if (instance? Component replaced-comp-or-node)
            (let [replaced-node (oget replaced-comp-or-node "inccup/node")]
              (goog.dom/replaceNode comp-node replaced-node)
              (recur replaced-comp-or-node replaced-node
                     keymap comp-moves))
            (goog.dom/removeNode replaced-comp-or-node)))
        (goog.dom/removeNode (oget comp "inccup/node"))))
    (goog.dom/removeNode (oget comp "inccup/node"))))

(defn handle-removed-comps [keymap comp-moves removed-comps]
  (loop [comp (.pop removed-comps)]
    (when comp
      (handle-moved-comps comp (oget comp "inccup/node")
                          keymap comp-moves)
      (recur (.pop removed-comps)))))

;; Mouvement clé existante -> anything mais pas
;; clé non existante (nouvel element) -> anything

;; remplacer par rien ou nouvel element avec clé ou element sans clé
;; mais pas element existant avec clé

(declare update-comp*)

(defn diff-children
  [element prev-forms index dynamic-nodes
   keymap comp-moves removed-comps]
  (let [prev-element (aget prev-forms index)]
    (cond
      (instance? Component element)
      (let [key (-> (oget element "inccup/opts")
                    (oget "key"))
            key (when key
                  (str (.-id element) key))]
        (if key
          (if-let [moved-comp (goog.object/get keymap key)]
            (do
              (when-not (identical? moved-comp prev-element)
                (cond
                  (instance? Component prev-element)
                  (add-comp-move prev-element key comp-moves)
                  (inccup-seq? prev-element)
                  (let [first-node (get-first-node prev-element)]
                    (goog.dom/insertSiblingBefore
                     (.createTextNode js/document "") first-node)
                    (pop-seq-from prev-element (aget dynamic-nodes index)
                                  0 (.-length prev-element) removed-comps)
                    (add-comp-move first-node key comp-moves))
                  :else
                  (add-comp-move (aget dynamic-nodes index)
                                 key comp-moves))
                (aset prev-forms index element)
                (aset dynamic-nodes index (oget moved-comp "inccup/node")))
              (update-comp* moved-comp element keymap
                            comp-moves removed-comps))
            (replace-element element prev-forms index dynamic-nodes
                             (create-comp* element keymap)
                             removed-comps))
          (if (and (instance? Component prev-element)
                   (= (.-id prev-element) (.-id element)))
            (update-comp* prev-element element keymap
                          comp-moves removed-comps)
            (replace-element element prev-forms index dynamic-nodes
                             (create-comp* element keymap)
                             removed-comps))))
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
                    (diff-children (first elements) prev-element @i nodes
                                   keymap comp-moves removed-comps)
                    (vswap! i inc)
                    (recur (rest elements)))
                  elements))]
          (when (< min-length prev-length)
            (pop-seq-from prev-element nodes min-length
                          prev-length removed-comps))
          (when (< min-length length)
            (let [parent (parent-node (aget nodes (dec @i)))]
              (loop [elements rest-elements]
                (when (and (< @i length) parent)
                  (create-dynamic parent (first elements)
                                  prev-element @i nodes keymap)
                  (vswap! i inc)
                  (recur (rest elements)))))))
        (let [parent (.-parentNode (aget dynamic-nodes index))]
          (replace-element element prev-forms index dynamic-nodes
                           (create-dynamic parent element prev-forms
                                           index dynamic-nodes keymap)
                           removed-comps)))
      (or (string? prev-element) (nil? prev-element))
      (when (not= prev-element (str element))
        (aset prev-forms index (str element))
        (oset (aget dynamic-nodes index) "nodeValue" (str element)))
      :else
      ;; element is a string or nil, prev-element is of different type
      (replace-element element prev-forms index dynamic-nodes
                       (.createTextNode js/document (str element))
                       removed-comps))))

(defn create-comp-elements
  "Walk the static tree of a component. Creates dom nodes during the walk.
   Returns the created node"
  [static forms forms-fn dynamic-nodes keymap]
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
              (create-dynamic new-node (forms-fn child) forms
                              child dynamic-nodes keymap)
              (string? child)
              (create-dynamic new-node child forms child
                              dynamic-nodes keymap)
              :else
              (->> (create-comp-elements child forms forms-fn
                                         dynamic-nodes keymap)
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
  [static var-deps-arr prev-forms forms-fn dynamic-nodes
   keymap comp-moves removed-comps]
  (cond
    (nil? static)
    nil
    (string? static)
    nil
    (number? static)
    ;; We just encountered a dynamic child. Diff it against the previous
    ;; child if the params it depends on did change.
    (when (aget var-deps-arr static)
      (diff-children (forms-fn static) prev-forms static dynamic-nodes
                     keymap comp-moves removed-comps))
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
                                keymap comp-moves removed-comps)
          (recur (inc index))))
      maybe-new-node)))

(defn create-comp* [comp keymap]
  (let [id (.-id comp)
        static (.-static$ comp)
        opts (oget comp "inccup/opts")
        key (oget opts "key")
        count-dynamic (.-count_dynamic comp)
        forms (-> count-dynamic make-array)
        var-deps-arr (-> count-dynamic make-array)
        dynamic-nodes (-> count-dynamic make-array)]
    (maybe-set-global id
                      "static" static
                      "var-deps" (.-var-deps comp))
    (oset comp "inccup/forms" forms)
    (oset comp "inccup/var-deps-arr" var-deps-arr)
    (oset comp "inccup/dynamic-nodes" dynamic-nodes)
    (when key
      (goog.object/set keymap (str id key) comp))
    (swap-count-global id inc)
    (let [new-node (create-comp-elements static forms (.-forms comp)
                                         dynamic-nodes keymap)]
      (oset comp "inccup/node" new-node)
      new-node)))

(defn update-comp* [prev-comp comp keymap comp-moves removed-comps]
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
                                             dynamic-nodes keymap
                                             comp-moves removed-comps)]
    (set! (.-params prev-comp) params)
    (when maybe-new-node
      (oset prev-comp "inccup/node" maybe-new-node))
    #_(clean-comp-keys prev-comp)))

(defn render! [node comp-fn & params]
  (binding [*globals* #js {:keymap #js {}}]
    (let [comp (apply comp-fn params)
          new-node (create-comp*
                    comp (goog.object/get *globals* "keymap"))]
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
          comp-moves #js {}
          removed-comps #js []]
      (assert (= (.-id comp) (.-id prev-comp)))
      (update-comp* prev-comp comp keymap comp-moves removed-comps)
      (handle-removed-comps keymap comp-moves removed-comps)
      (.log js/console "keymap " (goog.object/get *globals* "keymap"))
      (.log js/console "comp-moves " comp-moves)
      (.log js/console "removed-comps " removed-comps)
      (clean-globals)
      prev-comp)))
