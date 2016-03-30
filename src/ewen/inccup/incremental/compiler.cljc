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

(comment
  (merge-shortcut-attributes (with-meta {:f "e"}
                               {::map-attrs {:g "g"}})
                             {:e "e"})

  (meta (merge-map-attributes (with-meta {:f "e"}
                                {::shortcut-attrs {:g "g"}})
                              {:e "e"}))
  )
