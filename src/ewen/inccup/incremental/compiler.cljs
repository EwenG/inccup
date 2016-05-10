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
  (create-comp [c])
  (update-comp [c prev-c]))

(declare Component)

(defn inccup-seq? [x]
  (and (array? x) (aget x "inccup/seq")))

(defn nil-inccup-seq-from [x index]
  (loop [index index
         l (count x)]
    (when (< index l)
      (aset x index nil)
      (recur (inc index) l))))

(declare diff-children)

(defn create-inccup-seq [element index form]
  (aset element index
        (doto #js [] (aset "inccup/seq" true)))
  (loop [inccup-seq (aget element index)
         form form
         index 0]
    (when-let [f (first form)]
      (diff-children inccup-seq index f)
      (recur inccup-seq (rest form) (inc index)))))

(defn diff-children [element index form]
  (let [prev-form (aget element index)]
    (cond (instance? Component form)
          (cond (instance? Component prev-form)
                (if (= (.-id prev-form) (.-id form))
                  (update-comp form prev-form)
                  (aset element index (create-comp form)))
                (inccup-seq? prev-form)
                (aset element index (create-comp form))
                (nil? prev-form)
                (aset element index (create-comp form))
                :else
                (aset element index (create-comp form)))
          (seq? form)
          (cond (inccup-seq? prev-form)
                (loop [form form
                       index 0]
                  (if-let [f (first form)]
                    (do (diff-children prev-form index f)
                        (recur (rest form) (inc index)))
                    (when (< index (count prev-form))
                      (nil-inccup-seq-from prev-form index))))
                (nil? prev-form)
                (create-inccup-seq element index form)
                (instance? Component prev-form)
                (create-inccup-seq element index form)
                :else
                (create-inccup-seq element index form))
          (nil? form)
          (cond (nil? prev-form)
                (aset element index nil)
                (instance? Component prev-form)
                (aset element index nil)
                (inccup-seq? prev-form)
                (aset element index nil)
                :else
                (aset element index nil))
          :else
          (cond (nil? prev-form)
                (aset element index (str form))
                (instance? Component prev-form)
                (aset element index (str form))
                (inccup-seq? prev-form)
                (aset element index (str form))
                :else
                (aset element index (str form))))))

(defn attrs->js [attrs]
  (apply js-obj (interleave (map name (keys attrs))
                            (map str (vals attrs)))))

(defn update-form-dispatch [element index form form-type]
  (case form-type
    "tag" (let [old-tag (aget element index)
                new-tag (name (form))]
            (when (not= old-tag new-tag)
              (aset element index new-tag)
              (aset element (inc index) (js-obj))))
    "maybe-attrs" (let [old-attrs (aget element index)
                        new-attrs (form)]
                    (aset element index (attrs->js new-attrs)))
    "or-attrs-child" (diff-children element index (form))
    "child" (diff-children element index (form))
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
  (create-comp [c]
    (loop [static (static)
           paths paths
           index 0]
      (if-let [path (aget paths index)]
        (let [dec-path-length (dec (count path))
              leaf (aget-in static path dec-path-length 0)]
          (update-form-dispatch leaf
                                (aget path dec-path-length)
                                (aget forms index)
                                (aget form-types index))
          (recur static paths (inc index)))
        (set! value static)))
    c)
  (update-comp [_ prev-comp]
    (loop [tree @prev-comp
           paths paths
           index 0]
      (if-let [path (aget paths index)]
        (do
          (when-not (identical-params? (aget var-deps index)
                                       (.-params prev-comp)
                                       params)
            (let [dec-path-length (dec (count path))
                  leaf (aget-in tree path dec-path-length 0)]
              (update-form-dispatch leaf
                                    (aget path dec-path-length)
                                    (aget forms index)
                                    (aget form-types index))))
          (recur tree paths (inc index)))
        prev-comp))))

(comment

  )
