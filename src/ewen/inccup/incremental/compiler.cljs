(ns ewen.inccup.incremental.compiler
  (:require [clojure.string :as str]
            [ewen.inccup.util :as util]
            [goog.object]
            [goog.dom]))

(def ^:dynamic *tmp-val* nil)
(def ^:dynamic *globals* nil)

(defn array-with-path [path arr]
  (aset arr "inccup/update-paths" path)
  arr)

(defn maybe-merge-attributes [tag-attrs expr]
  (set! *tmp-val* expr)
  (if (map? expr)
    (util/merge-attributes tag-attrs expr)
    tag-attrs))

(defn aget-in [arr path count index]
  (if (< index count)
    (recur (aget arr (aget path index)) path count (inc index))
    arr))

(defprotocol IComponent
  (set-key [c key level]))

(declare Component)

(declare walk-children-comps)

(defn init-keymap [comp max-level]
  (loop [keymap (aset comp "inccup/keymap" (js-obj))
         removed-keys (aset comp "inccup/removed-keys" (js-obj))
         i 1]
    (when (<= i max-level)
      (aset keymap i (js-obj))
      (aset removed-keys i (js-obj))
      (recur keymap removed-keys (inc i)))))

(defn update-parent-comps [parent-comps comp max-level]
  (if (nil? (aget parent-comps 0))
    (do
      (aset parent-comps 0 (doto (make-array max-level) (aset 0 comp)))
      (aset parent-comps 1 0))
    (let [parents (aget parent-comps 0)
          new-index (inc (aget parent-comps 1))
          new-index (if (>= new-index (count parents)) 0 new-index)]
      (aset parents new-index comp)
      (aset parents parent-comps new-index))))

(defn parent-comp [parent-comps level]
  (let [parents (aget parent-comps 0)
        index (aget parent-comps 1)
        index (- index (dec level))
        index (if (< index 0) (+ index (count parents)) index)]
    (aget parents index)))

(defn register-comp-key [parent-comps comp]
  (let [parents (aget parent-comps 0)
        index (aget parent-comps 1)
        keymap (-> (aget parents index)
                   (aget "inccup/keymap")
                   (aget (.-level comp)))]
    (aset keymap (.-key comp) comp)))

(defn remove-comp-key [parent-comps comp]
  (let [parents (aget parent-comps 0)
        index (aget parent-comps 1)
        removed-keys (-> (aget parents index)
                         (aget "inccup/removed-keys")
                         (aget (.-level comp)))]
    (aset removed-keys (.-key comp) nil)))

(defn update-key-on-move [parent-comps comp]
  (let [parents (aget parent-comps 0)
        index (aget parent-comps 1)
        removed-keys (-> (aget parents index)
                         (aget "inccup/removed-keys")
                         (aget (.-level comp)))]
    (goog.object/remove removed-keys (.-key comp))))

(defn clean-comp-keys [comp max-level]
  (let [keymap (aget comp "inccup/keymap")
        removed-keys (aget comp "inccup/removed-keys")]
    (loop [max-level max-level
           i 1]
      (when (<= i max-level)
        (let [keymap (aget keymap i)
              removed-keys (aget removed-keys i)]
          (goog.object/forEach
           removed-keys (fn [_ k _]
                          (goog.object/remove removed-keys k)
                          (goog.object/remove keymap k))))
        (recur max-level (inc i))))))

(defn inccup-seq? [x]
  (and (array? x) (aget x "inccup/seq")))

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
          (aset "inccup/seq" true)
          (aset "inccup/seq-parent" element)
          (aset "inccup/seq-index" index)))
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
         update-path (aget parent "inccup/update-path")
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
                                       (aget "inccup/keymap")
                                       (aget level)
                                       (aget key))]
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
  (apply js-obj (interleave (map name (keys attrs))
                            (map str (vals attrs)))))

(defn identical-params? [prev-params params deps-indexes]
  (loop [index 0]
    (if-let [deps-index (aget deps-indexes index)]
      (if (identical? (aget prev-params deps-index)
                      (aget params deps-index))
        (recur (inc index))
        false)
      true)))

(deftype Component [id max-level static params var-deps
                    ^:mutable key ^:mutable level ^:mutable forms]
  IDeref
  (-deref [_] forms)
  IComponent
  (set-key [c k l]
    (set! key k)
    (set! level l)
    c))

(defn next-sibling [node]
  (when node (.-nextSibling node)))

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
             (or (aget attrs "name")
                 (aget attrs "type")))
          (let [tag-arr #js ["<" tag]]
            (when-let [attr-name (aget attrs "name")]
              (.push
               tag-arr " name=\"" (goog.string/htmlEscape attr-name) "\""))
            (when-let [attr-type (aget attrs "type")]
              (.push
               tag-arr " type=\"" (goog.string/htmlEscape attr-type) "\""))
            (.push tag-arr ">")
            (.join tag-arr ""))
          tag)
        element (.createElement js/document tag)]
    (goog.object/forEach
     attrs (fn [v k o]
             (if-let [prop-name (attr-as-prop k)]
               (aset element prop-name (aget attrs k))
               (.setAttribute element k (aget attrs k)))))
    (when children
      (loop [child (aget children 0)]
        (when child
          (.appendChild element child)
          (recur (.-nextSibling child)))))
    element))

(defn replace-or-append [parent new-node node]
  (if node
    (.replaceChild parent new-node node)
    (.appendChild parent new-node))
  new-node)

(defn get-or-set-global [id k v]
  (if *globals*
    (if-let [comp-globals (aget *globals* id)]
      (or (aget comp-globals k)
          (aset comp-globals k v))
      (do
        (aset *globals* id (js-obj k v "count" 0))
        v))
    v))

(defn inc-comp-global [id]
  (when *globals*
    (when-let [comp-globals (aget *globals* id)]
      (->> (aget comp-globals "count") inc
           (aset comp-globals "count")))))

(defn dec-comp-global [id]
  (when *globals*
    (when-let [comp-globals (aget *globals* id)]
      (->> (aget comp-globals "count") dec
           (aset comp-globals "count")))))

(defn clean-globals []
  (when *globals*
    (goog.object/forEach
     *globals* (fn [v k o]
                 (when (= 0 (aget v "count"))
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

(defn delete-prev-element [parent node prev-element]
  (cond
    (nil? prev-element) node
    (inccup-seq? prev-element)
    (loop [node node
           index 0]
      (if-let [e (aget prev-element index)]
        (recur (delete-prev-element parent node e)
               (inc index))
        node))
    :else ;; Text node or component
    (when node
      (let [next-node (next-sibling node)]
        (.removeChild parent node)
        next-node))))

(declare create-comp*)
(declare update-comp*)

(defn inccup-seq []
  (doto #js [] (aset "inccup/seq" true)))

(defn pop-inccup-seq-from [parent node x index]
  (let [l (count x)
        next-node (loop [node node
                         index index]
                    (if (< index l)
                      (recur (delete-prev-element
                              parent node (aget x index))
                             (inc index))
                      node))]
    (loop [index index]
      (when (< index l)
        (.pop x)
        (recur (inc index))))
    next-node))

(defn diff-children
  [parent node prev-element element prev-forms index]
  (cond
    (instance? Component element)
    (if (and (instance? Component prev-element)
             (= (.-id element) (.-id prev-element)))
      (update-comp* prev-element element parent node)
      (do
        (aset prev-forms index element)
        (.insertBefore parent (create-comp* element) node)
        (delete-prev-element parent node prev-element)))
    (seq? element)
    (if (inccup-seq? prev-element)
      (loop [node node
             elements element
             i 0]
        (if-not (empty? elements)
          (let [e (first elements)
                prev-e (aget prev-element i)]
            (recur (diff-children parent node prev-e e prev-element i)
                   (rest elements) (inc i)))
          (if (< i (count prev-element))
            (pop-inccup-seq-from parent node prev-element i)
            node)))
      (loop [node node
             elements element
             inccup-seq (inccup-seq)
             i 0]
        (if-not (empty? elements)
          (recur (diff-children parent node nil (first elements)
                                inccup-seq i)
                 (rest elements) inccup-seq (inc i))
          (do
            (aset prev-forms index inccup-seq)
            (delete-prev-element parent node prev-element)))))
    (nil? element)
    (do
      (aset prev-forms index nil)
      (delete-prev-element parent node prev-element))
    :else
    (if (or (instance? Component prev-element)
            (inccup-seq? prev-element)
            (nil? prev-element)
            (not= prev-element (str element)))
      (do
        (aset prev-forms index (str element))
        (.insertBefore
         parent (goog.dom/createTextNode (str element)) node)
        (delete-prev-element parent node prev-element))
      (next-sibling node))))

(defn create-comp-elements
  "Walk the static tree of a component. Creates dom nodes during the walk.
   Returns the created node"
  [static forms]
  (let [tag (if (number? (first static))
              (->> (first static) (aget forms) name
                   (aset forms (first static)))
              (first static))
        attrs (if (number? (second static))
                (->> (second static) (aget forms) attrs->js
                     (aset forms (second static)))
                (second static))]
    (let [new-node (create-dom tag attrs nil)
          l (count static)]
      (loop [index 2]
        (when (< index l)
          (let [child (aget static index)]
            (cond
              (nil? child)
              nil
              (number? child)
              (let [form (aget forms child)]
                (diff-children new-node nil nil form forms child))
              (string? child)
              (diff-children new-node nil nil child forms child)
              :else
              (->> (create-comp-elements child forms)
                   (.appendChild new-node))))
          (recur (inc index))))
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
             (let [prev-v (aget prev-attrs k)]
               (aset new-attrs-keys k nil)
               (when (not= prev-v v)
                 (.setAttribute node k v)
                 (aset prev-attrs k v)))))
    (goog.object/forEach
     prev-attrs (fn [v k _]
                  (when-not (goog.object/containsKey new-attrs-keys k)
                    (.removeAttribute node k)
                    (goog.object/remove prev-attrs k))))))

(defn update-comp-elements
  "Walk a component. Updates dynamic parts of the component during the
  walk. `node` is the current node beeing walked. `parent` is its parent
  node. Returns the next sibling of `node`"
  [parent node static var-deps-arr prev-forms forms]
  (cond
    (nil? static)
    node
    (number? static)
    ;; We just encountered a dynamic child. Diff it against the previous
    ;; child if the params it depends on did change.
    (let [prev-form (aget prev-forms static)]
      (if (aget var-deps-arr static)
        (diff-children parent node prev-form (aget forms static)
                       prev-forms static)
        (cond (nil? prev-form) node
              (inccup-seq? prev-form) (walk-inccup-seq node prev-form)
              :else (next-sibling node))))
    (string? static)
    (next-sibling node)
    :else
    (let [update-paths (aget static "inccup/update-paths")]
      (if (keep-walking-path? update-paths var-deps-arr)
        (let [tag (first static)
              prev-tag (if (number? tag) (aget prev-forms tag) tag)
              new-tag (if (and (number? tag) (aget var-deps-arr tag))
                        (name (aget forms tag))
                        prev-tag)
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
              maybe-new-node (if (not= prev-tag new-tag)
                               (replace-or-append
                                parent (create-dom new-tag new-attrs
                                                   (when node
                                                     (.-childNodes node)))
                                node)
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
          (loop [child (when maybe-new-node
                         (.-firstChild maybe-new-node))
                 index 2]
            (when (< index l)
              (recur
               (update-comp-elements
                maybe-new-node child (aget static index)
                var-deps-arr prev-forms forms)
               (inc index))))
          (next-sibling maybe-new-node))
        (next-sibling node)))))

(defn create-comp* [comp]
  (let [var-deps-arr (-> (.-var-deps comp) count make-true-arr)
        forms ((.-forms comp) var-deps-arr)]
    (set! (.-forms comp) forms)
    (aset comp "inccup/var-deps-arr" var-deps-arr)
    (inc-comp-global (.-id comp))
    (create-comp-elements (.-static$ comp) forms)))

(defn update-comp* [prev-comp comp parent node]
  (let [params (.-params comp)
        prev-params (.-params prev-comp)
        prev-forms (.-forms prev-comp)
        var-deps-arr (aget prev-comp "inccup/var-deps-arr")
        var-deps-arr (->> (.-var-deps comp)
                          (make-var-deps-arr
                           var-deps-arr params prev-params))
        forms ((.-forms comp) var-deps-arr)]
    (update-comp-elements parent node (.-static$ comp)
                          var-deps-arr prev-forms forms)))

(defn create-comp [comp node]
  (binding [*globals* (aset comp "inccup/globals" #js{})]
    (.appendChild node (create-comp* comp))
    comp))

(defn update-comp [prev-comp comp node]
  (binding [*globals* (aget prev-comp "inccup/globals")]
    (assert (not (nil? *globals*)))
    (assert (= (.-id comp) (.-id prev-comp)))
    (update-comp* prev-comp comp (.-parentNode node) node)
    (clean-globals)
    prev-comp))
