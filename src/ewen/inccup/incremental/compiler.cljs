(ns ewen.inccup.incremental.compiler
  (:require [clojure.string :as str]
            [ewen.inccup.util :as util]
            [goog.object]))

(def ^:dynamic *cache* nil)
(def ^:dynamic *implicit-param* nil)
(def ^:dynamic *version* nil)
(def ^:dynamic *tmp-val* nil)

(defn maybe-merge-attributes [tag-attrs expr]
  (set! *tmp-val* expr)
  (if (map? expr)
    (util/merge-attributes tag-attrs expr)
    tag-attrs))

(defn safe-aset [obj k v]
  (when obj (aset obj k v)))

(defn safe-aget [obj k]
  (when obj (aget obj k)))

(defn init-cache* []
  (js-obj "dynamic-counter" 0
          "dynamic-array" (array)))

(defn init-cache []
  (js-obj "sub-cache" (array)
          "version" 0))

(defn new-dynamic-cache [cache]
  (when cache
    (let [dynamic-counter (aget cache "dynamic-counter")
          current-cache (-> (aget cache "dynamic-array")
                            (aget dynamic-counter))]
      (aset cache "dynamic-counter" (inc dynamic-counter))
      (or current-cache
          (let [new-cache (init-cache)]
            (.push (aget cache "dynamic-array") new-cache)
            new-cache)))))

(defn get-static-cache [cache static-counter]
  (when cache
    (-> (aget cache "sub-cache")
        (aget static-counter))))

(defn make-static-cache [cache static-counter]
  (when cache
    (let [sub-cache (aget cache "sub-cache")]
      (when (> static-counter 0)
        (if (nil? (aget sub-cache 0))
          (dotimes [_ static-counter]
            (.push sub-cache (init-cache*)))
          (doseq [c sub-cache]
            (aset c "dynamic-counter" 0)))))))

(defn assoc-in-tree*
  [tree [[index & r-index :as path] & paths]
   skips preds-and-forms counter assoc-fn]
  (if (aget preds-and-forms @counter)
    (if r-index
      (assoc-fn tree index (assoc-in-tree*
                            (get tree index)
                            (conj paths r-index) skips
                            preds-and-forms counter assoc))
      (if paths
        (do (vswap! counter inc)
            (cond->> (loop [sub-tree (transient (if index
                                                  (get tree index)
                                                  tree))
                            [path & r-paths] paths]
                       (if path
                         (recur
                          (assoc-in-tree* sub-tree path skips
                                          preds-and-forms counter assoc!)
                          r-paths)
                         (persistent! sub-tree)))
              index (assoc-fn tree index)))
        (let [c (dec (vswap! counter + 2))]
          (assoc-fn tree index ((aget preds-and-forms c))))))
    (do
      (vswap! counter + (get skips @counter))
      tree)))

(defn assoc-in-tree [tree paths skips preds-and-forms]
  (assoc-in-tree* tree paths skips preds-and-forms (volatile! 0) assoc))

(comment

  (assoc-in-tree
   '["p" {} nil]
   '([] ([1]) ([2]))
   '[5 2 0 2 0]
   #js [true true 'x true 'y])

  (let [ee '[:div {} [[nil [[nil 6]]]]]]
    (identical?
     (get-in ee [2 0 1])
     (get-in (assoc-in-tree
              ee
              '([2 0] ([0]) ([1 0] ([0]) ([1])) ([2] ([0]) ([1])))
              '[8 2 0 5 2 0 2 0]
              #js [true true 'x false true 'y true 'z]) [2 0 1])))

  (assoc-in-tree
   '["p" {} nil]
   '([] ([1]) ([2]))
   '[5 2 0 2 0]
   #js ['x123 'x 'y123 'y])

  )

#_(defn merge-attrs [o m]
    (doseq [[k v] m]
      (let [k (name k)]
        (if (= "class" k)
          (aset o "class" (str (aget o "class") " " v))
          (aset o (name k) v))))
    o)

#_(defn normalize-element [[tag attrs]]
    (let [[_ tag id class] (re-matches util/re-tag (name tag))
          tag-attrs        (cond-> (js-obj)
                             id (doto (aset "id" id))
                             class (doto (aset
                                          "class"
                                          (if class
                                            (str/replace class "." " ")))))
          attrs (if (map? attrs) (merge-attrs tag-attrs attrs) tag-attrs)]
      #js [tag attrs]))

(defn text-element? [e]
  (or (string? e) (keyword? e)
      (symbol? e) (number? e)))

(comment
  (delete-element component parent prev index element)
  (create-element component parent prev index element)
  (create-component component parent prev index component)
  (delete-element component parent prev index component)
  (update-element component parent prev index old new)
  (update-component component parent prev index old new)
  (attr-update component parent prev index element key value new-value)

  )

#_(defn remove-elements-from-to [parent prev from to]
    (loop [index from
           prev prev]
      (when (< index to)
        #_(delete-element component parent prev index (aget parent index))
        (recur (inc index) nil)))
    (loop [index from]
      (when (< index to)
        (.pop parent)
        (recur (inc index)))))

#_(defn component? [e]
    (aget e "inccup/component"))

#_(defn sub-component? [e]
    (aget e "inccup/sub-component"))

#_(defn match-component-types? [e new-e]
    (identical? e (aget new-e "inccup/static")))

#_(defn diff-element [parent prev index new-e]
    (let [[tag attrs :as e] (aget parent index)]
      (cond
        #_(and (nil? e) (component? new-e))
        #_(let [new-e (new-e first-render)]
            (aset parent index new-e)
            (create-component component parent prev index new-e)
            new-e)
        #_(and (component? e) (component? new-e))
        #_(if (match-component-types? e new-e)
            (let [new-e (new-e e)]
              (aset parent index new-e)
              new-e)
            (let [new-e (new-e first-render)]
              (when (component? e)
                (delete-component component parent prev index e))
              (aset parent index new-e)
              (when (component? new-e)
                (create-component component parent prev index new-e))
              new-e))
        ;; e is not a component
        #_(component? new-e)
        #_(let [new-e (new-e first-render)]
            (delete-element component parent prev index e)
            (aset parent index (new-e first-render))
            (create-component component parent prev index new-e)
            new-e)
        #_(and (component? e) (vector? new-e))
        #_(let [new-e (normalize-element new-e)]
            (delete-component component parent prev index e)
            (aset parent index new-e)
            (create-element component parent prev index new-e)
            new-e)
        #_(and (component? e) (text-element? new-e))
        #_(let [new-e (str new-e)]
            (delete-component component parent prev index e)
            (aset parent index new-e)
            (create-element component parent prev index new-e)
            new-e)
        ;; e can only be nil when the parent element has new children
        (and (nil? e) (vector? new-e))
        (let [new-e (normalize-element new-e)]
          (aset parent index new-e)
          #_(create-element component parent prev index new-e)
          new-e)
        (and (nil? e) (text-element? new-e))
        (let [new-e (str new-e)]
          (aset parent index new-e)
          #_(create-element component parent prev index new-e)
          new-e)
        (and (array? e) (vector? new-e))
        (let [[new-tag new-attrs :as new-e]
              (normalize-element new-e)]
          (if (and (not= tag new-tag))
            (do
              #_(delete-element component parent prev index e)
              (aset parent index new-e)
              #_(create-element component parent prev index new-e)
              new-e)
            (do
              (goog.object/forEach
               new-attrs (fn [new-v new-k new-o]
                           (let [v (aget attrs new-k)]
                             (when (not= v new-v)
                               (aset attrs new-k new-v)
                               #_(attr-update component parent prev index e
                                              new-k v new-v)))))
              (goog.object/forEach
               attrs (fn [v k o]
                       (when-not (aget new-attrs k)
                         (goog.object/remove attrs k)
                         #_(attr-update component parent prev index
                                        e k v nil))))
              e)))
        (and (text-element? e) (text-element? new-e))
        (let [new-e (str new-e)]
          (if (= e new-e)
            e
            (do
              (aset parent index new-e)
              #_(update-element component parent prev index e new-e)
              new-e)))
        (vector? new-e) ;;text-element <--> vector
        (let [new-e (normalize-element new-e)]
          #_(delete-element component parent prev index e)
          (aset parent index new-e)
          #_(create-element component parent prev index new-e)
          new-e)
        (text-element? new-e) ;;array <--> text-element
        (let [new-e (str new-e)]
          #_(delete-element component parent prev index e)
          (aset parent index new-e))
        :else (throw
               (js/Error.
                (str "Invalid elements: " e " " new-e))))))

;; TODO Handle components diffing
#_(defn inccup-diff [parent prev index new-form]
    (if (or (nil? new-form)
            (and (seq? new-form) (empty? new-form)))
      (let [l (count parent)]
        (if (>= index l)
          prev
          (do
            (remove-elements-from-to parent prev index l)
            prev)))
      (cond
        (seq? new-form)
        (let [[f & rest] new-form
              updated (diff-element parent prev index f)]
          (recur parent updated (inc index) rest))
        (vector? new-form)
        (let [updated (diff-element parent prev index new-form)
              maybe-attrs (second new-form)]
          (let [index (if (or (nil? maybe-attrs) (map? maybe-attrs))
                        2 1)]
            (loop [[new-form & next-forms] (drop index new-form)
                   prev nil
                   index index]
              (if new-form
                (recur next-forms
                       (inccup-diff updated prev index new-form)
                       (inc index))
                (inccup-diff updated prev index nil))))
          (recur parent updated (inc index) nil))
        :else
        (let [updated (diff-element parent prev index new-form)]
          (recur parent updated (inc index) nil)))))

(defn vector-children [vector]
  (let [maybe-attrs (second vector)]
    (if (or (nil? maybe-attrs) (map? maybe-attrs))
      (rest (rest vector))
      (rest vector))))

(defn diff-first-form [forms]
  (loop [f (first forms)]
    (if (seq? f)
      (recur (first f))
      f)))

(defn diff-rest-forms [forms]
  (let [peeked (peek forms)]
    (if (seq? peeked)
      (if (seq peeked)
        (conj (pop forms) (diff-rest-forms (peek forms)))
        (pop forms))
      (if (seq forms)
        (pop forms)
        '()))))

(defn merge-attrs [attrs id class]
  (cond-> attrs
    id (assoc :id id)
    class (assoc :class (str (get attrs :class) " " class))))

(defn normalize-element [[tag attrs]]
  (let [[_ tag id class] (re-matches util/re-tag (name tag))
        class        (when class
                       (str/replace class "." " "))
        attrs (if (map? attrs)
                (merge-attrs attrs id class)
                {:id id :class class})]
    #js [tag attrs]))

(defn create-element [e]
  (if (vector? e)
    (let [normalized (normalize-element e)]
      e)
    (str e)))

(defn create-element-rec [e]
  (let [[f & rest] e]
    (cond (vector? f)
          (do (create-element f)
              (recur rest))
          (nil? f)
          (recur rest)
          (seq? f)
          (do (create-element-rec f)
              (recur rest))
          :else (do (create-element f)
                    (recur rest)))))

(defn delete-element [e]
  nil)


#_(let [[new-tag new-attrs :as new-e]
        (normalize-element new-e)]
    (if (and (not= tag new-tag))
      (do
        #_(delete-element component parent prev index e)
        (aset parent index new-e)
        #_(create-element component parent prev index new-e)
        new-e)
      (do
        (goog.object/forEach
         new-attrs (fn [new-v new-k new-o]
                     (let [v (aget attrs new-k)]
                       (when (not= v new-v)
                         (aset attrs new-k new-v)
                         #_(attr-update component parent prev index e
                                        new-k v new-v)))))
        (goog.object/forEach
         attrs (fn [v k o]
                 (when-not (aget new-attrs k)
                   (goog.object/remove attrs k)
                   #_(attr-update component parent prev index
                                  e k v nil))))
        e)))

(defn diff-vectors [v1 v2]
  (let [[v1-tag v1-attrs :as v1] (normalize-element v1)
        [v2-tag v2-attrs :as v2] (normalize-element v2)]
    (if (not= v1-tag v2-tag)
      (do (delete-element v1)
          (create-element v2)
          (create-element-rec (vector-children v2)))
      nil)))

(defn inccup-diff [prev-forms new-forms]
  (let [prev-first (diff-first-form prev-forms)
        prev-rest (diff-rest-forms prev-forms)
        new-first (diff-first-form new-forms)
        new-rest (diff-rest-forms new-forms)]
    (cond (vector? new-first)
          (cond (vector? prev-first)
                (do (diff-vectors prev-first new-first)
                    (inccup-diff (vector-children prev-first)
                                 (vector-children new-rest)))
                (nil? prev-first)
                (do (create-element new-first)
                    (create-element-rec (vector-children prev-first)))
                :else
                (do (delete-element prev-first)
                    (create-element new-first)
                    (create-element-rec (vector-children prev-first))))
          (nil? new-first)
          (if (nil? prev-first)
            nil
            (delete-element prev-first))
          :else
          (when-not (= prev-first new-first)
            (when-not (nil? prev-first)
              (delete-element prev-first))
            (create-element  new-first)))
    (if (or (seq prev-rest) (seq new-rest))
      (recur prev-rest new-rest)
      nil)))

(comment

  (let [tree '#js ["div"]]
    (inccup-diff
     tree nil 1 '[:p {} ([:div.cc] "e")])
    tree)

  (let [tree '#js ["div"]]
    (inccup-diff
     tree nil 1 "e")
    tree)

  (let [tree '#js ["div" [:p] [:div]]]
    (inccup-diff
     tree nil 1 "e")
    tree)

  (let [tree '#js ["div" #js ["p" #js {:a "a" :b "b"} "r" [:div#r "p"]]
                   "r"]]
    (inccup-diff
     tree nil 1 [:p {:a "a2" :c "c2"}])
    tree)
  )

(defn clean-cache [cache]
  (let [version (aget cache "version")
        sub-cache (aget cache "sub-cache")]
    (doseq [c sub-cache]
      (let [dyn-arr (aget c "dynamic-array")
            f-item  (aget dyn-arr 0)]
        (when (and f-item (= version (aget f-item "version")))
          (loop [l (dec (count dyn-arr))]
            (when-let [l-item (aget dyn-arr l)]
              (if (< (aget l-item "version") version)
                (.pop dyn-arr)
                (clean-cache l-item))
              (recur (dec l)))))))))

(comment

  (copy-cache
   #js {"dynamic-counter" 0
        "dynamic-array"
        #js [#js {"sub-cache"
                  #js [#js {"dynamic-counter" 0
                            "dynamic-array"
                            #js [#js {"sub-cache" #js []
                                      "prev-result" #js ["p" #js {} 1]
                                      "params" #js [1]}]}]
                  "prev-result" #js ["p" #js {} 1]
                  "params" #js [1]}]})

  )

(defn apply-update-fn [update-fn prev-val prev-params params params-nb]
  (let [update-fn-params (array)]
    (.push update-fn-params prev-val)
    (dotimes [i params-nb]
      (.push update-fn-params (aget params i)))
    (if prev-params
      (dotimes [i params-nb]
        (.push update-fn-params
               (not (identical? (aget params i) (aget prev-params i)))))
      (dotimes [i params-nb]
        (.push update-fn-params true)))
    (apply update-fn update-fn-params)))

#_(defn inccupdate
    [update-fn params params-nb cache-static-counter]
    (let [version (or *version* (inc (aget *cache* "version")))
          cache (-> (new-dynamic-cache *implicit-param*)
                    (or *cache*))
          _ (aset cache "version" version)
          _ (set! *version* nil)
          _ (set! *implicit-param* nil)
          _ (make-static-cache cache cache-static-counter)
          prev-params (safe-aget cache "params")
          update-fn-params (compute-update-fn-params
                            cache params prev-params params-nb)
          result (apply update-fn update-fn-params)]
      (safe-aset cache "params" params)
      (safe-aset cache "prev-result" result)
      result))

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
