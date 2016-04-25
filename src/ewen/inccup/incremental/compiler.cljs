(ns ewen.inccup.incremental.compiler
  (:require [clojure.string :as str]
            [ewen.inccup.util :as util]))

(def ^:dynamic *cache* nil)
(def ^:dynamic *implicit-param1* nil)
(def ^:dynamic *implicit-param2* nil)
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

(defn init-cache []
  (js-obj "dynamic-counter" 0
          "dynamic-array" (array)))

(defn inc-dynamic-cache [cache]
  (when cache
    (let [dynamic-counter (aget cache "dynamic-counter")]
      (aset cache "dynamic-counter" (inc dynamic-counter))
      dynamic-counter)))

(defn new-dynamic-cache [cache counter]
  (when cache
    (let [dynamic-array (aget cache "dynamic-array")
          cache-item (aget dynamic-array counter)]
      (or cache-item
          (aset dynamic-array counter
                (js-obj "sub-cache" (array)))))))

(let [empty-obj (js-obj "sub-cache" (array))]
  (defn get-dynamic-cache [cache counter]
    (when cache
      (let [current-cache (-> (aget cache "dynamic-array")
                              (aget counter))]
        (or current-cache empty-obj)))))

(defn get-static-cache [cache static-counter]
  (when cache
    (-> (aget cache "sub-cache")
        (aget static-counter))))

(defn make-static-cache [cache static-counter]
  (when cache
    (let [sub-cache (aget cache "sub-cache")]
      (when (and (> static-counter 0)
                 (nil? (aget sub-cache 0)))
        (dotimes [_ static-counter]
          (.push sub-cache (init-cache)))))))

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
          (assoc-fn tree index (aget preds-and-forms c)))))
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

(defn copy-cache [cache]
  (when cache
    (let [dyn-array (aget cache "dynamic-array")
          first-dynamic (aget dyn-array 0)]
      #js {"dynamic-counter" 0
           "dynamic-array"
           (if first-dynamic
             #js [(let [new-sub-cache #js []]
                    (doseq [sub-cache (aget first-dynamic "sub-cache")]
                      (.push new-sub-cache (copy-cache sub-cache)))
                    #js {"sub-cache" new-sub-cache
                         "prev-result" (aget first-dynamic "prev-result")
                         "params" (aget first-dynamic "params")})]
             #js [])})))

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

(defn compute-update-fn-params
  [prev-cache cache params prev-params params-nb]
  (let [update-fn-params (array)]
    (.push update-fn-params prev-cache)
    (.push update-fn-params cache)
    (dotimes [i params-nb]
      (.push update-fn-params (aget params i)))
    (if prev-params
      (dotimes [i params-nb]
        (.push update-fn-params
               (not= (aget params i) (aget prev-params i))))
      (dotimes [i params-nb]
        (.push update-fn-params true)))
    update-fn-params))

(defn inccupdate
  [update-fn params params-nb cache-static-counter]
  (let [prev-cache (or *implicit-param1* *cache*)
        cache (or *implicit-param2*
                  (->> (copy-cache prev-cache)
                       (set! *cache*)))
        _ (set! *implicit-param1* nil)
        _ (set! *implicit-param2* nil)
        dynamic-counter (inc-dynamic-cache cache)
        prev-cache (get-dynamic-cache prev-cache dynamic-counter)
        cache (new-dynamic-cache cache dynamic-counter)
        _ (make-static-cache prev-cache cache-static-counter)
        _ (make-static-cache cache cache-static-counter)
        prev-params (safe-aget prev-cache "params")
        update-fn-params (compute-update-fn-params
                          prev-cache cache
                          params prev-params params-nb)
        result (apply update-fn update-fn-params)]
    (safe-aset cache "params" params)
    (safe-aset cache "prev-result" result)
    result))

(defn update-with-cache [static prev-cache cache static-counter
                         update-path skips preds-and-exprs]
  (let [prev-cache (get-static-cache prev-cache static-counter)
        cache (get-static-cache cache static-counter)
        dynamic-counter (inc-dynamic-cache cache)
        prev-cache (get-dynamic-cache prev-cache dynamic-counter)
        cache (new-dynamic-cache cache dynamic-counter)
        result (-> (safe-aget prev-cache "prev-result")
                   (or static)
                   (assoc-in-tree update-path skips preds-and-exprs))]
    (safe-aset cache "prev-result" result)
    result))

(comment
  (merge-shortcut-attributes (with-meta {:f "e"}
                               {::map-attrs {:g "g"}})
                             {:e "e"})

  (meta (merge-map-attributes (with-meta {:f "e"}
                                {::shortcut-attrs {:g "g"}})
                              {:e "e"}))
  )
