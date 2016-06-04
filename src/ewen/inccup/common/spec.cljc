(ns ewen.inccup.common.spec
  (:require [ewen.inccup.compiler :as comp]
            #?(:clj [clojure.spec :as spec]
               :cljs [cljs.spec :as spec])))

;;Inccup forms

(defn map-with-only-keyword-keys? [m]
  (and (map? m)
       (every? keyword? (keys m))))

(spec/def ::comp/tag (spec/or :string string?
                           :symbol symbol?
                           :keyword keyword?))
(spec/def ::comp/attrs map-with-only-keyword-keys?)
(spec/def ::comp/child (spec/or :child-form ::comp/form
                                :child-nil nil?
                                :child-string string?))
(spec/def ::comp/form
  (spec/alt
   :no-attrs (spec/cat :tag ::comp/tag
                       :children (spec/* ::comp/child))
   :with-attrs (spec/cat :tag ::comp/tag
                         :attrs ::comp/attrs
                         :children (spec/* ::comp/child))))

(comment
  (spec/valid? ::comp/form [:div nil "e"])
  )
