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

(defn apply-update-fn [update-fn prev-result params params-nb static]
  (let [first-render? (identical? static prev-result)
        prev-params (aget (meta prev-result) "inccup/prev-params")
        update-fn-params (array)]
    (.push update-fn-params prev-result)
    (.push update-fn-params first-render?)
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

(comment
  (merge-shortcut-attributes (with-meta {:f "e"}
                               {::map-attrs {:g "g"}})
                             {:e "e"})

  (meta (merge-map-attributes (with-meta {:f "e"}
                                {::shortcut-attrs {:g "g"}})
                              {:e "e"}))
  )
