(ns ewen.inccup.incremental.compiler
  (:require [clojure.string :as str]))

(def ^:dynamic *cache* nil)
(def ^:dynamic *implicit-param* nil)

(def ^{:doc "Regular expression that parses a CSS-style id and class from
an element name."
       :private true}
  re-tag #"([^\s\.#]+)(?:#([^\s\.#]+))?(?:\.([^\s#]+))?")

(defn unevaluated?
  "True if the expression has not been evaluated."
  [expr]
  (or (symbol? expr)
      (and (seq? expr)
           (not= (first expr) `quote))))

(defn merge-attributes [attrs1 {:keys [id] :as attrs2}]
  (let [merged-attrs (merge-with
                      #(cond (nil? %1) %2
                             (unevaluated? %2) `(str ~%1 " " ~%2)
                             :else (str %1 " " %2))
                      attrs1 attrs2)]
    (if id (assoc merged-attrs :id id) merged-attrs)))

(defn normalize-element
  "Ensure an element vector is of the form [tag-name attrs content]."
  [[tag & content]]
  (when (not (or (keyword? tag) (symbol? tag) (string? tag)))
    (throw (js/Error. (str tag " is not a valid element name."))))
  (let [[_ tag id class] (re-matches re-tag (name tag))
        tag-attrs        (cond-> {}
                           id (assoc :id id)
                           class (assoc
                                  :class
                                  (if class
                                    (str/replace ^String class "." " "))))
        map-attrs        (first content)]
    (if (map? map-attrs)
      [tag (merge-attributes tag-attrs map-attrs) (next content)]
      [tag tag-attrs content])))

(defn merge-shortcut-attributes [attrs1 attrs2]
  (let [attrs1-meta (meta attrs1)
        map-attrs (::map-attrs attrs1-meta)
        merged-attrs (merge-with
                      #(cond (nil? %1) %2
                             (unevaluated? %2) `(str ~%1 " " ~%2)
                             :else (str %1 " " %2))
                      map-attrs attrs2)]
    (with-meta merged-attrs (assoc attrs1-meta ::shortcut-attrs attrs2))))

(defn merge-map-attributes [attrs1 {:keys [id] :as attrs2}]
  (let [attrs1-meta (meta attrs1)
        shortcut-attrs (::shortcut-attrs attrs1-meta)
        merged-attrs (merge-with
                      #(cond (nil? %1) %2
                             (unevaluated? %2) `(str ~%1 " " ~%2)
                             :else (str %1 " " %2))
                      shortcut-attrs attrs2)
        merged-attrs (if id (assoc merged-attrs :id id) merged-attrs)]
    (with-meta merged-attrs (assoc attrs1-meta ::map-attrs attrs2))))

(defn safe-aset [obj k v]
 (when obj (aset obj k v)))

(defn safe-aget [obj k]
 (when obj (aget obj k)))

(defn init-cache []
 ;; The top-level field is used to clean the dynamic arrays
 ;; of the top level component when it terminates its execution. It is
 ;; useful because the dynamic arrays are cleaned by the previous
 ;; components, but the top-level component has no previous component
 (js-obj "sub-cache" (array) "top-level" true))

(defn init-cache* []
 ;; The top-level field is used to clean the dynamic arrays
 ;; of the top level component when it terminates its execution. It is
 ;; useful because the dynamic arrays are cleaned by the previous
 ;; components, but the top-level component has no previous component
 (js-obj "dynamic-counter" 0
         "dynamic-array" (array)))

(defn new-dynamic-cache [cache]
 (when cache
   (let [dynamic-counter (aget cache "dynamic-counter")
         ;; The dynamic counter of the top level component will never
         ;; be more than one. We don't increment it. It avoids the need
         ;; to clean it. Cleaning it is tricky because the top-level
         ;; component has no predecessor and dynamic array is cleaned
         ;; by the parent component.
         is-top-level? (aget cache "top-level")
         current-cache (-> (aget cache "dynamic-array")
                           (aget dynamic-counter))]
     (when-not is-top-level?
       (aset cache "dynamic-counter" (inc dynamic-counter)))
     (or current-cache
         (let [new-cache (js-obj "sub-cache" (array))]
           (.push (aget cache "dynamic-array") new-cache)
           new-cache)))))

(defn clean-dynamic-array [c]
 (let [dyn-arr (aget c "dynamic-array")
       dyn-counter (aget c "dynamic-counter")
       cache-shrink-nb (- (count dyn-arr) dyn-counter)]
   ;; dyn counter can be 0 either because the sub-component was not
   ;; called because the var(s) it depends on did not change or because
   ;; it is inside a loop that was executed on an empty collection.
   ;; In the first case, we must not clean the dynamic array.
   ;; The fact that we don't clean the array in the second case is an
   ;; unfortunate side effect, but I can't see a simple way to avoid it.
   (when (not= dyn-counter 0)
     (dotimes [_ cache-shrink-nb]
       (.pop dyn-arr))
     (aset c "dynamic-counter" 0))))

(defn clean-sub-cache [cache]
 (when cache
   (doseq [c (aget cache "sub-cache")]
     (clean-dynamic-array c))))

(defn get-static-cache [cache static-counter]
 (when cache
   (-> (aget cache "sub-cache")
       (aget static-counter))))

(defn make-static-cache [cache static-counter]
 (when cache
   (let [sub-cache (aget cache "sub-cache")]
     (when (not= static-counter (count sub-cache))
       (dotimes [_ static-counter]
         (.push sub-cache (init-cache*)))))))

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

(defn compute-update-fn-params [cache params prev-params params-nb]
 (let [update-fn-params (array)]
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
 (let [cache (or (new-dynamic-cache *implicit-param*) *cache*)
       prev-params (safe-aget cache "params")
       update-fn-params (compute-update-fn-params
                         cache params prev-params params-nb)
       _ (make-static-cache cache cache-static-counter)
       result (apply update-fn update-fn-params)]
   (safe-aset cache "params" params)
   (safe-aset cache "prev-result" result)
   (clean-sub-cache cache)
   (set! *implicit-param* nil)
   result))

(defn update-with-cache [static cache static-counter update-path
                         skips preds-and-exprs]
  (let [cache (get-static-cache cache static-counter)
        cache (new-dynamic-cache cache)
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
