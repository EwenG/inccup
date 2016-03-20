(ns ewen.inccup.compiler-data)

(def ^:dynamic *cache* nil)

(def ^{:doc "A list of elements that must be rendered without a closing
tag."
       :private true}
  void-tags
  #{"area" "base" "br" "col" "command" "embed" "hr" "img" "input" "keygen"
    "link" "meta" "param" "source" "track" "wbr"})

(defn- container-tag?
  "Returns true if the tag has content or is not a void tag."
  [tag content]
  (or content (not (void-tags tag))))
