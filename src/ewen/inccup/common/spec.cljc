(ns ewen.inccup.common.spec
  (:require [ewen.inccup.compiler :as comp]
            #?(:clj [clojure.spec :as spec]
               :cljs [cljs.spec :as spec])))


;; Options
(spec/def ::comp/string-output-mode #(= :string %))
(spec/def ::comp/output-mode #{:string :incremental})
(spec/def ::comp/options (spec/keys :opt [::comp/output-mode]))

(spec/fdef comp/h
           :args (spec/alt :no-option ::spec/any
                           :with-options (spec/cat
                                          :forms ::spec/any
                                          :options ::comp/options)))

#?(:clj (spec/instrument #'comp/h))

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
