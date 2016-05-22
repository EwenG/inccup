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

(defn pop-inccup-seq-from
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

(declare diff-children)

(defn create-inccup-seq [element index form parent-comps
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

(defn walk-seq-comps [inccup-seq comp-fn direction]
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

(defn walk-children-comps [comp comp-fn direction]
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

(defn delete-prev-form
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

(defn next-sibling [node]
  (when node (.-nextSibling node)))

(defn delete-prev-element [parent node prev-element]
  (let [next-node (if prev-element (next-sibling node) node)]
    (cond
      (instance? Component prev-element)
      (when node (.removeChild parent node))
      (nil? prev-element) nil
      (seq? prev-element)
      (loop [node node
             elements prev-element]
        (when-let [e (first elements)]
          (recur (delete-prev-element parent node e)
                 (rest elements))))
      :else
      (when node (.removeChild parent node)))
    next-node))

(defn copy-unchanged-forms [var-deps-arr forms prev-forms]
  (let [l (count var-deps-arr)]
    (loop [index 0]
      (when (< index l)
        (when-not (aget var-deps-arr index)
          (aset forms index (aget prev-forms index)))
        (recur (inc index))))))

(defn replace-node-with-children [parent new-node node]
  (loop [child (.-firstChild node)]
    (when child
      (.appendChild new-node child)
      (recur (.-firstChild node))))
  (.replaceChild parent new-node node))

(declare create-comp*)
(declare update-comp*)

(defn diff-children
  [parent node element prev-element]
  (cond
    (instance? Component element)
    (if (and (instance? Component prev-element)
             (= (.-id element) (.-id prev-element)))
      (update-comp* parent node element prev-element)
      (do
        (.insertBefore parent (create-comp* element) node)
        (delete-prev-element parent node prev-element)))
    (seq? element)
    (if (seq? prev-element)
      (loop [node node
             elements element
             prev-elements prev-element]
        (let [e (first elements)
              prev-e (first prev-elements)]
          (when (or e prev-e)
            (recur (diff-children parent node e prev-e)
             (rest elements) (rest prev-elements)))))
      (do
        (loop [elements element]
          (when-let [e (first elements)]
            (diff-children parent node e nil)
            (recur (rest element))))
        (delete-prev-element parent node prev-element)))
    (nil? element)
    (delete-prev-element parent node prev-element)
    :else
    (if (or (instance? Component prev-element)
            (seq? prev-element)
            (nil? prev-element)
            (not= (str prev-element) (str element)))
      (do
        (.insertBefore
         parent (goog.dom/createTextNode (str element)) node)
        (delete-prev-element parent node prev-element)))))

(defn create-comp-elements
  "Walk the static tree of a component. Creates dom nodes during the walk.
   Returns the create node"
  [static forms]
  (let [tag (if (number? (first static))
              (->> (first static) (aget forms) name)
              (first static))
        attrs (if (number? (second static))
                (->> (second static) (aget forms) attrs->js)
                (second static))]
    (let [new-node (goog.dom/createDom tag attrs)
          l (count static)]
      (loop [index 2]
        (when (< index l)
          (let [child (aget static index)]
            (cond
              (nil? child)
              nil
              (number? child)
              (let [form (aget forms child)]
                (diff-children new-node nil form nil))
              (string? static)
              (->> (goog.dom/createTextNode static)
                   (.appendChild new-node))
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

(defn walk-seq [node seq]
  (loop [node node
         seq seq]
    (if-let [e (first seq)]
      (cond (nil? e) (recur node (rest seq))
            (seq? e) (do (walk-seq node e)
                         (recur node (rest seq)))
            :else (recur (next-sibling node) (rest seq)))
      node)))

(defn diff-attrs [node attrs prev-attrs]
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

(defn update-comp-elements
  "Walk a component. Updates dynamic parts of the component during the
  walk. `node` is the current node beeing walked. `parent` is its parent
  node. Returns the next sibling of `node`"
  [parent node static var-deps-arr forms prev-forms]
  (cond
    (nil? static)
    node
    (number? static)
    ;; We just encountered a dynamic child. Diff it against the previous
    ;; child if the params it depends on did change.
    (let [form (aget forms static)]
      (if (aget var-deps-arr static)
        (let [prev-form (aget prev-forms static)]
          (diff-children parent node form prev-form))
        (cond (nil? form) node
              (seq? form) (walk-seq node form)
              :else (next-sibling node))))
    (string? static)
    (next-sibling node)
    :else
    (let [update-paths (aget static "inccup/update-paths")]
      (if (keep-walking-path? update-paths var-deps-arr)
        (let [tag (first static)
              attrs (second static)
              ;; If the tag did change, replace the current node by a
              ;; node of a new type and move the children of the old node
              ;; to the new one
              maybe-new-node
              (if (and (number? tag)
                       (not= (name (aget forms tag))
                             (name (aget prev-forms tag))))
                (let [attrs (if (number? attrs)
                              (attrs->js (aget forms attrs))
                              attrs)
                      new-node (goog.dom/createDom
                                (name (aget forms tag)) attrs)]
                  (if node
                    (replace-node-with-children parent new-node node)
                    (.appendChild parent new-node))
                  new-node)
                node)]
          (let [l (count static)]
            ;; Update the node attributes if the params it depends on
            ;; did change or if the node tag did change
            (when (or (not (identical? maybe-new-node node))
                      (and (number? attrs) (aget var-deps-arr attrs)))
              (diff-attrs
               node (get forms attrs) (get prev-forms attrs)))
            (loop [child (when maybe-new-node
                           (.-firstChild maybe-new-node))
                   index 2]
              (when (< index l)
                (recur
                 (update-comp-elements
                  node child (aget static index)
                  var-deps-arr forms prev-forms)
                 (inc index))))
            (next-sibling maybe-new-node)))
        (next-sibling node)))))

(defn create-comp* [comp]
  (let [var-deps-arr (-> (.-var-deps comp) count make-true-arr)
        forms ((.-forms comp) var-deps-arr)]
    (set! (.-forms comp) forms)
    (inc-comp-global (.-id comp))
    (aset comp "inccup/var-deps-arr" var-deps-arr)
    (create-comp-elements (.-static$ comp) forms)))

(defn update-comp* [prev-comp comp parent node]
  (let [var-deps (.-var-deps comp)
        params (.-params comp)
        prev-params (.-params prev-comp)
        prev-forms (.-forms prev-comp)
        var-deps-arr (aget prev-comp "inccup/var-deps-arr")
        var-deps-arr (->> (.-var-deps comp)
                          (make-var-deps-arr
                           var-deps-arr params prev-params))
        forms ((.-forms comp) var-deps-arr)]
    (copy-unchanged-forms var-deps-arr forms prev-forms)
    (update-comp-elements parent node (.-static$ comp)
                          var-deps-arr forms prev-forms)))

(defn create-comp [comp node]
  (binding [*globals* (aset comp "inccup/globals" #js{})]
    (.appendChild node (create-comp* comp))
    comp))

(defn update-comp [prev-comp comp node]
  (binding [*globals* (aget prev-comp "inccup/globals")]
    (assert (not (nil? *globals*)))
    (assert (= (.-id comp) (.-id prev-comp)))
    (aset comp "inccup/globals" *globals*)
    (update-comp* prev-comp comp (.-parentNode node) node)
    (clean-globals)
    comp))
