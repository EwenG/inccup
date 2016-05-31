(ns ewen.inccup.common.runtime)

(def void-tags
  "A list of elements that must be rendered without a
  closing tag."
  #{"area" "base" "br" "col" "command" "embed" "hr" "img" "input"
    "keygen" "link" "meta" "param" "source" "track" "wbr"})

(defn merge-attributes [tag-attrs expr]
  (let [id (:id expr)
        merged-attrs (merge-with
                      (fn [attr1 attr2]
                        (if attr1 (str attr1 " " attr2) attr2))
                      tag-attrs expr)]
    (if id (assoc merged-attrs :id id) merged-attrs)))
