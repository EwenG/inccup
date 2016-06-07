(ns ewen.inccup.common.spec
  (:require [ewen.inccup.compiler :as comp]
            #?(:clj [clojure.spec :as spec]
               :cljs [cljs.spec :as spec])
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen
             #?@(:cljs [:include-macros true])]
            [clojure.test.check.properties :as prop]))

;;Inccup forms

(def element-name-re #"[a-zA-Z][a-zA-Z0-9]*")
(def attr-key-re #"[a-zA-Z][a-zA-Z0-9\-]*")

(defn valid-element-name? [e-name]
  (and (keyword? e-name)
       (re-matches element-name-re (name e-name))))

(defn valid-attr-key? [attr-k]
  (and (keyword? attr-k)
       (re-matches attr-key-re (name attr-k))))

(defn dyn-element? [x]
  (or (symbol? x) (seq x)))

(spec/def ::comp/tag (spec/or :literal-tag valid-element-name?
                              :dynamic-tag symbol?))
(spec/def ::comp/attr string?)
(spec/def ::comp/attrs (spec/map-of
                        (spec/or :literal-attr-key valid-attr-key?
                                 :dynamic-attr-key dyn-element?)
                        (spec/or :literal-attr-val ::comp/attr
                                 :dynamic-attr-val dyn-element?)))
(spec/def ::comp/child (spec/alt :child-form ::comp/form
                                 :child-nil nil?
                                 :child-string string?
                                 :dynamic-child dyn-element?))

(spec/def ::comp/form
  (spec/and
   vector?
   (spec/cat :tag ::comp/tag
             :attrs (spec/? ::comp/attrs)
             :children (spec/* ::comp/child))))

(comment
  (spec/valid? ::comp/form [:div {:data-yy "e"}
                            '("r" [:p {:r "r"} nil
                                   ("e" [:div])] nil) "r"])
  )
