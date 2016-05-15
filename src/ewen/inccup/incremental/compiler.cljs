(ns ewen.inccup.incremental.compiler
  (:require [clojure.string :as str]
            [ewen.inccup.util :as util]
            [goog.object]))

(def ^:dynamic *tmp-val* nil)

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
  (loop [value (.-value comp)
         paths (.-paths comp)
         index 0]
    (when-let [path (aget paths index)]
      (let [dec-path-length (dec (count path))
            leaf (aget-in value path dec-path-length 0)]
        (let [index-in-parent (aget path dec-path-length)
              element (aget leaf index-in-parent)]
          (cond (instance? Component element)
                (if (= "top-bottom" direction)
                  (do (comp-fn leaf index-in-parent element)
                      (walk-children-comps element comp-fn direction))
                  (do (walk-children-comps element comp-fn direction)
                      (comp-fn leaf index-in-parent element)))
                (inccup-seq? element)
                (walk-seq-comps element comp-fn direction)
                :else nil))
        (recur value paths (inc index))))))

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
                (create-element element index (str form))
                (aset element index (str form)))))))

(defn attrs->js [attrs]
  (apply js-obj (interleave (map name (keys attrs))
                            (map str (vals attrs)))))

(defn diff-attrs [element index new-attrs update-attribute]
  (let [old-attrs (aget element index)]
    (aset element index (attrs->js new-attrs))
    (let [new-attrs-keys (js-obj)]
      (loop [attrs-keys (keys new-attrs)]
        (when-let [k (first attrs-keys)]
          (let [k-name (name k)
                old-v (aget old-attrs k-name)
                v (str (get new-attrs k))]
            (aset new-attrs-keys k-name nil)
            (when (not= old-v v)
              (aset old-attrs k-name v)
              (update-attribute element index k-name old-v v))
            (recur (rest attrs-keys)))))
      (goog.object/forEach
       old-attrs (fn [v k _]
                   (when-not (goog.object/containsKey new-attrs-keys k)
                     (goog.object/remove old-attrs k)
                     (update-attribute element index k v nil)))))))

(defn update-form-dispatch [element index form form-type
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

(defn create-static [parent index static create-element]
  (when static
    (create-element parent index static)
    (let [maybe-attrs (aget static 1)]
      (loop [index (if (and (not (array? maybe-attrs))
                            (not (nil? maybe-attrs))
                            (not (string? maybe-attrs)))
                     2 1)]
        (when-let [next-child (aget static index)]
          (create-static static index next-child create-element))))))

#_(defn clone-with-path [static path index]
  (if (< index (count path))
    (let [current-vec (if (aget static "inccup/cloned")
                        static
                        (doto (aclone static)
                          (aset "inccup/cloned" true)))]
      (doto current-vec
        (aset (aget path index)
              (clone-with-path (aget static (aget path index))
                               path (inc index)))))
    static))

#_(defn clone-with-paths [static paths]
  (loop [paths paths
         index 0]
    (when-let [path (aget paths index)]
      (clone-with-path static path 0)
      (recur paths (inc index))))
    static)

(defn identical-params? [deps-indexes prev-params params]
  (loop [deps-indexes deps-indexes
         index 0]
    (if-let [deps-index (aget deps-indexes index)]
      (if (identical? (aget prev-params deps-index)
                      (aget params deps-index))
        (recur deps-indexes (inc index))
        false)
      true)))

(deftype Component [static paths var-deps form-types forms params id
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
      (create-static parent index static create-element)
      (loop [static static
             paths paths
             index 0]
        (if-let [path (aget paths index)]
          (let [dec-path-length (dec (count path))
                leaf (aget-in static path dec-path-length 0)]
            (update-form-dispatch leaf
                                  (aget path dec-path-length)
                                  (aget forms index)
                                  (aget form-types index)
                                  parent-comps
                                  update-tag update-attribute
                                  remove-element create-element
                                  move-comp
                                  will-update did-update
                                  mount-comp unmount-comp)
            (recur static paths (inc index)))
          (set! value static))))
    (when key
        (register-comp-key parent-comps c))
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
    (loop [tree @prev-comp
           paths paths
           index 0]
      (when-let [path (aget paths index)]
        (when-not (identical-params? (aget var-deps index)
                                     (.-params prev-comp)
                                     params)
          (let [dec-path-length (dec (count path))
                leaf (aget-in tree path dec-path-length 0)]
            (update-form-dispatch leaf
                                  (aget path dec-path-length)
                                  (aget forms index)
                                  (aget form-types index)
                                  parent-comps
                                  update-tag update-attribute
                                  remove-element create-element
                                  move-comp
                                  will-update did-update
                                  mount-comp unmount-comp)))
        (recur tree paths (inc index))))
    (clean-comp-keys prev-comp max-level)
    (did-update c)
    prev-comp)
  (set-key [c k l]
    (set! key k)
    (set! level l)
    c))
