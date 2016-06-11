(ns ewen.inccup.common.runtime)

(def ^:dynamic *attrs-or-first-child* nil)

(defn merge-attributes [tag-attrs expr]
  (let [id (:id expr)
        merged-attrs (merge-with
                      (fn [attr1 attr2]
                        (if attr1 (str attr1 " " attr2) attr2))
                      tag-attrs expr)]
    (if id (assoc merged-attrs :id id) merged-attrs)))

(defn maybe-merge-attributes [tag-attrs expr]
  (set! *attrs-or-first-child* expr)
  (if (map? expr)
    (merge-attributes tag-attrs expr)
    tag-attrs))

(defn maybe-first-child []
  (if (map? *attrs-or-first-child*)
    nil *attrs-or-first-child*))
