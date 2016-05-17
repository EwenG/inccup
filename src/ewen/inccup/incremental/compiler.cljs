(ns ewen.inccup.incremental.compiler
  (:require [clojure.string :as str]
            [ewen.inccup.util :as util]
            [goog.object]))

(def ^:dynamic *tmp-val* nil)

(defn array-with-meta [update-path dynamic arr]
  (aset arr "inccup/update-path" update-path)
  (aset arr "inccup/dynamic" dynamic)
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
  (create-comp [c parent index parent-comps
                update-tag update-attribute
                remove-element create-element move-comp
                will-update did-update
                mount-comp unmount-comp])
  (update-comp [c prev-c parent-comps
                update-tag update-attribute
                remove-element create-element move-comp
                will-update did-update
                mount-comp unmount-comp])
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

  (defn diff-children [element index form parent-comps
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

(defn diff-attrs [element index new-attrs update-attribute]
  (let [old-attrs (aget element index)]
    (let [new-attrs-keys (js-obj)]
      (loop [attrs-keys (keys new-attrs)]
        (when-let [k (first attrs-keys)]
          (let [k-name (name k)
                old-v (aget old-attrs k-name)
                v (str (get new-attrs k))]
            (aset new-attrs-keys k-name nil)
            (when (not= old-v v)
              (update-attribute element index k-name old-v v)
              (aset old-attrs k-name v))
            (recur (rest attrs-keys)))))
      (goog.object/forEach
       old-attrs (fn [v k _]
                   (when-not (goog.object/containsKey new-attrs-keys k)
                     (update-attribute element index k v nil)
                     (goog.object/remove old-attrs k)))))))

#_(defn update-form-dispatch [element index form form-type
                            parent-comps
                            update-tag update-attribute
                            remove-element create-element
                            move-comp
                            will-update did-update
                            mount-comp unmount-comp]
  (case form-type
    "tag" (let [old-tag (aget element index)
                new-tag (name (form))]
            (when (not= old-tag new-tag)
              (aset element index new-tag)
              (update-tag element old-tag new-tag)))
    "maybe-attrs" (diff-attrs element index (form) update-attribute)
    "or-attrs-child" (diff-children element index (form) parent-comps
                                    update-tag update-attribute
                                    remove-element create-element
                                    move-comp
                                    will-update did-update
                                    mount-comp unmount-comp)
    "attrs" (diff-attrs element index (form) update-attribute)
    "child" (diff-children element index (form) parent-comps
                           update-tag update-attribute
                           remove-element create-element
                           move-comp
                           will-update did-update
                           mount-comp unmount-comp)
    (throw (js/Error. (str "Invalid form-type: " form-type)))))

(defn identical-params? [prev-params params deps-indexes]
  (loop [deps-indexes deps-indexes
         index 0]
    (if-let [deps-index (aget deps-indexes index)]
      (if (identical? (aget prev-params deps-index)
                      (aget params deps-index))
        (recur deps-indexes (inc index))
        false)
      true)))

(defn create-static [parent index static create-element]
  (when static
    (create-element parent index static false)
    (let [maybe-attrs (aget static 1)]
      (loop [index 2
             l (count static)]
        (when (< index static)
          (let [next-child (aget static index)]
            (create-static static index next-child create-element)
            (recur (inc index) l)))))))

(defn create-comp-elements
  [parent index static forms form-index update-path
   parent-comps
   update-tag update-attribute
   remove-element create-element
   move-comp
   will-update did-update
   mount-comp unmount-comp]
  (let [dynamic (aget static "inccup/dynamic")]
    (loop [index 0
           update-path-index 0
           next-index (aget update-path update-path-index)
           l (count static)]
      (when (< index l)
        (if (> index 1)
          (let [next-child (aget static index)]
            (if (= index next-index)
              (do
                (if-let [next-update-path
                         (when next-child
                           (aget next-child "inccup/update-path"))]
                  (create-comp-elements static index next-child
                                        forms form-index next-update-path
                                        parent-comps
                                        update-tag update-attribute
                                        remove-element create-element
                                        move-comp
                                        will-update did-update
                                        mount-comp unmount-comp)
                  (let [form ((aget forms @form-index))]
                    (vswap! form-index inc)
                    (diff-children static index form
                                   parent-comps
                                   update-tag update-attribute
                                   remove-element create-element
                                   move-comp
                                   will-update did-update
                                   mount-comp unmount-comp)))
                (recur (inc index) (inc update-path-index)
                       (aget update-path (inc update-path-index)) l))
              (do
                (create-static static index next-child create-element)
                (recur (inc index) update-path-index
                       next-index l))))
          (cond
            (= next-index 1)
            (do
              (aset static 1 (attrs->js ((aget forms @form-index))))
              (vswap! form-index inc)
              (recur (inc index) (inc update-path-index)
                     (aget update-path (inc update-path-index)) l))
            (= next-index 0)
            (do
              (aset static 0 (name ((aget forms @form-index))))
              (vswap! form-index inc)
              (recur (inc index) (inc update-path-index)
                     (aget update-path (inc update-path-index)) l))
            (= index 1)
            (do
              (create-element parent index static dynamic)
              (recur (inc index) update-path-index
                     next-index l))
            :else (recur (inc index) update-path-index
                         next-index l)))))))

(defn update-comp-elements
  [static forms var-deps form-index
   identical-params? update-path
   parent-comps
   update-tag update-attribute
   remove-element create-element move-comp
   will-update did-update
   mount-comp unmount-comp]
  (loop [update-path-index 0
         l (count update-path)]
    (when (< update-path-index l)
      (let [index (aget update-path update-path-index)]
        (cond (> index 1)
              (let [next-child (aget static index)]
                (if-let [next-update-path
                         (when next-child
                           (aget next-child "inccup/update-path"))]
                  (update-comp-elements next-child forms var-deps
                                        form-index identical-params?
                                        next-update-path
                                        parent-comps
                                        update-tag update-attribute
                                        remove-element create-element
                                        move-comp
                                        will-update did-update
                                        mount-comp unmount-comp)
                  (do
                    (when-not (identical-params?
                               (aget var-deps @form-index))
                      (let [new-element ((aget forms @form-index))]
                        (diff-children static index
                                       new-element parent-comps
                                       update-tag update-attribute
                                       remove-element create-element
                                       move-comp
                                       will-update did-update
                                       mount-comp unmount-comp)))
                    (vswap! form-index inc)
                    (recur (inc update-path-index) l))))
              (= index 1)
              (do
                (when-not (identical-params? (aget var-deps @form-index))
                  (diff-attrs static 1 ((aget forms @form-index))
                              update-attribute))
                (vswap! form-index inc)
                (recur (inc update-path-index) l))
              (= index 0)
              (do
                (let [old-tag (aget static 0)
                      new-tag (name ((aget forms @form-index)))]
                  (when-not (= old-tag new-tag)
                    (update-tag static old-tag new-tag)
                    (aset static 0 new-tag)))
                (vswap! form-index inc)
                (recur (inc update-path-index) l)))))))

(deftype Component [static var-deps forms params id
                    ^:mutable key ^:mutable level max-level
                    ^:mutable value]
  IDeref
  (-deref [_] value)
  IComponent
  (create-comp [c parent index parent-comps
                update-tag update-attribute
                remove-element create-element move-comp
                will-update did-update
                mount-comp unmount-comp]
    (init-keymap c max-level)
    (update-parent-comps parent-comps c max-level)
    (let [static (static)]
      (create-comp-elements parent index static forms (volatile! 0)
                            (or (aget static "inccup/update-path")
                                #js [])
                            parent-comps
                            update-tag update-attribute
                            remove-element create-element
                            move-comp
                            will-update did-update
                            mount-comp unmount-comp)
      (set! value static))
    (when key (register-comp-key parent-comps c))
    (mount-comp parent index c)
    (walk-children-comps c mount-comp "top-bottom")
    c)
  (update-comp [c prev-comp parent-comps
                update-tag update-attribute
                remove-element create-element move-comp
                will-update did-update
                mount-comp unmount-comp]
    (will-update c)
    (update-parent-comps (or parent-comps #js []) prev-comp max-level)
    (update-comp-elements
     @prev-comp forms var-deps (volatile! 0)
     (partial identical-params? (.-params prev-comp) params)
     (aget @prev-comp "inccup/update-path")
     parent-comps
     update-tag update-attribute
     remove-element create-element
     move-comp
     will-update did-update
     mount-comp unmount-comp)
    (clean-comp-keys prev-comp max-level)
    (did-update c)
    prev-comp)
  (set-key [c k l]
    (set! key k)
    (set! level l)
    c))
