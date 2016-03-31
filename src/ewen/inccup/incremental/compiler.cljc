(ns ewen.inccup.incremental.compiler)

(def ^:dynamic *cache* nil)

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

#?(:cljs
   (defn init-cache []
     (js-obj "dynamic-counter" 0
             "dynamic-array" (array))))

#?(:cljs
   (defn get-or-set-cache [cache]
     (when cache
       (let [dynamic-counter (aget cache "dynamic-counter")
             current-cache (-> (aget cache "dynamic-array")
                               (aget dynamic-counter))]
         (aset cache "dynamic-counter" (inc dynamic-counter))
         (or current-cache
             (let [new-cache (js-obj "sub-cache" (array))]
               (.push (aget cache "dynamic-array") new-cache)
               new-cache))))))

#?(:cljs
   (defn clean-sub-cache [cache]
     (doseq [c (aget cache "sub-cache")]
       (let [dyn-arr (aget c "dynamic-array")
             cache-shrink-nb (- (count dyn-arr)
                                (aget c "dynamic-counter"))]
         (dotimes [_ (range cache-shrink-nb)]
           (.pop dyn-arr)))
       (aset c "dynamic-counter" 0))))

(comment
  (merge-shortcut-attributes (with-meta {:f "e"}
                               {::map-attrs {:g "g"}})
                             {:e "e"})

  (meta (merge-map-attributes (with-meta {:f "e"}
                                {::shortcut-attrs {:g "g"}})
                              {:e "e"}))
  )
