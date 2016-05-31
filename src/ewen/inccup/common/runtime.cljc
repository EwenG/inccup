(ns ewen.inccup.common.runtime)

(defn merge-attributes [tag-attrs expr]
  (let [id (:id expr)
        merged-attrs (merge-with
                      (fn [attr1 attr2]
                        (if attr1 (str attr1 " " attr2) attr2))
                      tag-attrs expr)]
    (if id (assoc merged-attrs :id id) merged-attrs)))
