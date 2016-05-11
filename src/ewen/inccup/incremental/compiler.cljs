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

(defprotocol IComponent
  (create-comp [c
                update-tag update-attribute
                remove-element create-element
                will-update did-update
                mount-comp unmount-comp])
  (update-comp [c prev-c
                update-tag update-attribute
                remove-element create-element
                will-update did-update
                mount-comp unmount-comp]))

(declare Component)

(defn inccup-seq? [x]
  (and (array? x) (aget x "inccup/seq")))

(defn pop-inccup-seq-from [x index remove-element unmount-comp]
  (loop [index index
         l (count x)]
    (when (< index l)
      (.pop x)
      (recur (inc index) l))))

(declare diff-children)

(defn create-inccup-seq [element index form
                         update-tag update-attribute
                         remove-element create-element
                         will-update did-update
                         mount-comp unmount-comp]
  (aset element index
        (doto #js [] (aset "inccup/seq" true)))
  (loop [inccup-seq (aget element index)
         form form
         index 0]
    (when-let [f (first form)]
      (diff-children inccup-seq index f
                     update-tag update-attribute
                     remove-element create-element
                     will-update did-update
                     mount-comp unmount-comp)
      (recur inccup-seq (rest form) (inc index)))))

(defn diff-children [element index form
                     update-tag update-attribute
                     remove-element create-element
                     will-update did-update
                     mount-comp unmount-comp]
  (let [prev-form (aget element index)]
    (cond (instance? Component form)
          (cond (instance? Component prev-form)
                (if (= (.-id prev-form) (.-id form))
                  (update-comp form prev-form
                               update-tag update-attribute
                               remove-element create-element
                               will-update did-update
                               mount-comp unmount-comp)
                  (let [new-comp (create-comp
                                  form
                                  update-tag update-attribute
                                  remove-element create-element
                                  will-update did-update
                                  mount-comp unmount-comp)]
                    #_(unmount-comp element index (aget element index))
                    (aset element index new-comp)
                    #_(mount-comp element index new-comp)))
                (inccup-seq? prev-form)
                (let [new-comp (create-comp form
                                            update-tag update-attribute
                                            remove-element create-element
                                            will-update did-update
                                            mount-comp unmount-comp)]
                  (pop-inccup-seq-from
                   prev-form 0 remove-element unmount-comp)
                  (aset element index new-comp)
                  #_(mount-comp element index new-comp))
                (nil? prev-form)
                (let [new-comp (create-comp form
                                            update-tag update-attribute
                                            remove-element create-element
                                            will-update did-update
                                            mount-comp unmount-comp)]
                  (aset element index new-comp)
                  #_(mount-comp element index new-comp))
                :else
                (let [new-comp (create-comp
                                form
                                update-tag update-attribute
                                remove-element create-element
                                will-update did-update
                                mount-comp unmount-comp)]
                  (remove-element element index)
                  (aset element index new-comp)
                  #_(mount-comp element index new-comp)))
          (seq? form)
          (cond (inccup-seq? prev-form)
                (loop [form form
                       index 0]
                  (if-let [f (first form)]
                    (do (diff-children prev-form index f
                                       update-tag update-attribute
                                       remove-element create-element
                                       will-update did-update
                                       mount-comp unmount-comp)
                        (recur (rest form) (inc index)))
                    (when (< index (count prev-form))
                      (pop-inccup-seq-from
                       prev-form index remove-element unmount-comp))))
                (nil? prev-form)
                (create-inccup-seq element index form
                                   update-tag update-attribute
                                   remove-element create-element
                                   will-update did-update
                                   mount-comp unmount-comp)
                (instance? Component prev-form)
                (do
                  #_(unmount-comp element index prev-form)
                  (create-inccup-seq element index form
                                     update-tag update-attribute
                                     remove-element create-element
                                     will-update did-update
                                     mount-comp unmount-comp))
                :else
                (do
                  (remove-element element index)
                  (create-inccup-seq element index form
                                     update-tag update-attribute
                                     remove-element create-element
                                     will-update did-update
                                     mount-comp unmount-comp)))
          (nil? form)
          (cond (nil? prev-form)
                nil
                (instance? Component prev-form)
                (do
                  (unmount-comp element index prev-form)
                  (aset element index nil))
                (inccup-seq? prev-form)
                (do
                  (pop-inccup-seq-from
                   prev-form 0 remove-element unmount-comp)
                  (aset element index nil))
                :else
                (do
                  (remove-element element index)
                  (aset element index nil)))
          :else
          (cond (nil? prev-form)
                (do
                  (aset element index (str form))
                  (create-element element index (str form)))
                (instance? Component prev-form)
                (do
                  #_(unmount-comp element index prev-form)
                  (aset element index (str form))
                  (create-element element index (str form)))
                (inccup-seq? prev-form)
                (do
                  (pop-inccup-seq-from
                   prev-form 0 remove-element unmount-comp)
                  (aset element index (str form))
                  (create-element element index (str form)))
                :else
                (do
                  (remove-element element index)
                  (aset element index (str form))
                  (create-element element index (str form)))))))

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
                   (when-not (aget new-attrs-keys k)
                     (goog.object/remove old-attrs k)
                     (update-attribute element index k v nil)))))))

(defn update-form-dispatch [element index form form-type
                            update-tag update-attribute
                            remove-element create-element
                            will-update did-update
                            mount-comp unmount-comp]
  (case form-type
    "tag" (let [old-tag (aget element index)
                new-tag (name (form))]
            (when (not= old-tag new-tag)
              (aset element index new-tag)
              (update-tag element old-tag new-tag)))
    "maybe-attrs" (diff-attrs element index (form) update-attribute)
    "or-attrs-child" (diff-children element index (form)
                                    update-tag update-attribute
                                    remove-element create-element
                                    will-update did-update
                                    mount-comp unmount-comp)
    "attrs" (diff-attrs element index (form) update-attribute)
    "child" (diff-children element index (form)
                           update-tag update-attribute
                           remove-element create-element
                           will-update did-update
                           mount-comp unmount-comp)
    (throw (js/Error. (str "Invalid form-type: " form-type)))))

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

(defn aget-in [arr path count index]
  (if (< index count)
    (recur (aget arr (aget path index)) path count (inc index))
    arr))

(defn identical-params? [deps-indexes prev-params params]
  (loop [deps-indexes deps-indexes
         index 0]
    (if-let [deps-index (aget deps-indexes index)]
      (if (identical? (aget prev-params deps-index)
                      (aget params deps-index))
        (recur deps-indexes (inc index))
        false)
      true)))

(deftype Component [static paths var-deps form-types forms
                    params id ^:mutable value]
  IDeref
  (-deref [_] value)
  IComponent
  (create-comp [c
                update-tag update-attribute
                remove-element create-element
                will-update did-update
                mount-comp unmount-comp]
    (loop [static (static)
           paths paths
           index 0]
      (if-let [path (aget paths index)]
        (let [dec-path-length (dec (count path))
              leaf (aget-in static path dec-path-length 0)]
          (update-form-dispatch leaf
                                (aget path dec-path-length)
                                (aget forms index)
                                (aget form-types index)
                                update-tag update-attribute
                                remove-element create-element
                                will-update did-update
                                mount-comp unmount-comp)
          (recur static paths (inc index)))
        (set! value static)))
    c)
  (update-comp [_ prev-comp
                update-tag update-attribute
                remove-element create-element
                will-update did-update
                mount-comp unmount-comp]
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
                                  update-tag update-attribute
                                  remove-element create-element
                                  will-update did-update
                                  mount-comp unmount-comp)))
        (recur tree paths (inc index))))
    prev-comp))

(comment

  )
