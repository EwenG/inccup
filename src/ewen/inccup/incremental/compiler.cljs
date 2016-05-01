(ns ewen.inccup.incremental.compiler
  (:require [clojure.string :as str]
            [ewen.inccup.util :as util]))

(def ^:dynamic *cache* nil)
(def ^:dynamic *implicit-param* nil)
(def ^:dynamic *version* nil)
(def ^:dynamic *tmp-val* nil)

(defn maybe-merge-attributes [tag-attrs expr]
  (let [tag-attrs (or tag-attrs *tmp-val*)]
    (set! *tmp-val* expr)
    (if (map? expr)
      (util/merge-attributes tag-attrs *tmp-val* )
      tag-attrs)))

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

(defn inccup-assoc-in [tree paths skips preds-and-forms]
  )

(defn merge-attrs [o m]
  (doseq [[k v] m]
    (let [k (name k)]
      (if (= "class" k)
        (aset o "class" (str (aget o "class") " " v))
        (aset o (name k) v))))
  o)

(defn normalize-element [[tag attrs]]
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
  (update-element component parent prev index old new)
  (attr-update component parent prev index element key value new-value)

  )

(defn remove-elements-from-to [parent prev from to]
  (prn from " " to)
  (loop [index from
         prev prev]
    (when (< index to)
      #_(delete-element component parent prev index (aget parent index))
      (recur (inc index) nil)))
  (loop [index from]
    (when (< index to)
      (.pop parent)
      (recur (inc index)))))

(defn diff-element [parent prev index new-e]
  (let [[tag attrs :as e] (aget parent index)]
    (cond
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

(defn inccup-diff [parent prev index new-form]
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

(def first-render (js-obj))

(defn apply-update-fn [update-fn prev-result params params-nb static]
  (let [prev-params (aget prev-result "inccup/prev-params")
        update-fn-params (array)]
    (.push update-fn-params prev-result)
    (.push update-fn-params)
    (if prev-params
      (dotimes [i params-nb]
        (.push update-fn-params
               (not= (aget params i) (aget prev-params i))))
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

(defn update-with-cache [static cache static-counter
                         update-path skips preds-and-exprs]
  (let [version (aget cache "version")
        cache (-> (get-static-cache cache static-counter)
                  new-dynamic-cache)
        _ (safe-aset cache "version" version)
        result (-> (safe-aget cache "prev-result")
                   (or static)
                   (assoc-in-tree update-path skips preds-and-exprs))]
    (safe-aset cache "prev-result" result)
    result))
