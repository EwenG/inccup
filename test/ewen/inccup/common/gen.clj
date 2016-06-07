(ns ewen.inccup.common.gen
  (:require [ewen.inccup.compiler :as comp :refer [h]]
            [ewen.inccup.common.spec]
            [clojure.spec :as spec]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [cljs.analyzer.api :as ana-api]
            [cljs.compiler.api :as comp-api]))

(def tag-name-gen
  (gen/fmap (fn [[f-char r-chars]]
              (-> (into [f-char] r-chars) clojure.string/join keyword))
            (gen/tuple gen/char-alpha (gen/vector gen/char-alphanumeric))))

(def attr-name-gen
  (gen/fmap (fn [[f-char r-chars]]
              (-> (into [f-char] r-chars) clojure.string/join keyword))
            (gen/tuple gen/char-alpha
                       (gen/vector
                        (gen/one-of [gen/char-alphanumeric
                                     (gen/return\-)])))))

(def keyword-gen (gen/one-of [gen/keyword gen/keyword-ns]))
(def html-tag-gen
  (gen/elements
   #{:a :abbr :address :area :article :aside :audio :b :base :bdi :bdo
     :big :blockquote :body :br :button :canvas :caption :cite :code :col
     :colgroup :data :datalist :dd :del :details :dfn :dialog :div :dl :dt
     :em :embed :fieldset :figcaption :figure :footer :form :h1 :h2 :h3 :h4
     :h5 :h6 :head :header :hgroup :hr :html :i :iframe :img :input :ins
     :kbd :keygen :label :legend :li :link :main :map :mark :menu :menuitem
     :meta :meter :nav :noscript :object :ol :optgroup :option :output :p
     :param :picture :pre :progress :q :rp :rt :ruby :s :samp :script
     :section :select :small :source :span :strong :style :sub :summary :sup
     :table :tbody :td :textarea :tfoot :th :thead :time :title :tr :track
     :u :ul :var :video :wbr}))

(defn make-tag-gen [tag-syms]
  (if (empty? tag-syms)
    (gen/one-of [tag-name-gen html-tag-gen])
    (gen/one-of [tag-name-gen html-tag-gen
                 (gen/elements tag-syms)])))

(def attrs-gen (gen/map attr-name-gen gen/string))
(def child-gen (gen/one-of [gen/string (gen/return nil)]))
(def child-seq-gen (gen/fmap (fn [children] `(list ~@children))
                             (gen/list child-gen)))

(defn make-form-container-gen [tag-gen]
  (fn [child-gen]
    (gen/let [tag tag-gen
              attr attrs-gen
              children (gen/vector
                        (gen/one-of
                         [child-gen child-seq-gen]))]
      (into [tag attr] children))))

(defn make-form-gen [tag-gen]
  (gen/recursive-gen (make-form-container-gen tag-gen) child-gen))

(def string-alpha
  (gen/fmap clojure.string/join
            (gen/not-empty (gen/vector gen/char-alpha))))

(def syms-gen (gen/fmap symbol string-alpha))

(def tag-syms-gen
  (gen/map syms-gen
           ;; max is the max number of values for a given tag symbol param
           (gen/vector tag-name-gen 1 20)
           ;; Number of tag symbols parameters
           {:max-elements 5}))

(def attr-vals-gen
  (gen/map syms-gen (gen/vector gen/string 1 20)
           {:max-elements 5}))

(comment
  (gen/sample tag-syms-gen)
  )

(def params-gen (gen/let [tag-syms tag-syms-gen
                          attr-vals attr-vals-gen]
                  {:tags tag-syms
                   :attr-vals attr-vals}))

(comment
  (gen/sample params-gen)
  )

(def form-gen
  (gen/let [{:keys [tags attr-vals] :as params} params-gen]
    (let [tag-syms (keys tags)
          attr-vals-syms (keys attr-vals)
          tag-vals (map first (vals tags))
          attr-vals-vals (map first (vals attr-vals))
          tag-gen (make-tag-gen tag-syms)]
      (gen/fmap
       (fn [form]
         {:params params
          :form form
          :string-clj `(apply
                        (fn [~@tag-syms ~@attr-vals-syms]
                          (h ~form))
                        [~@tag-vals ~@attr-vals-vals])
          :string-cljs
          (binding [comp/*cljs-output-mode* :string]
            (comp-api/emit
             (ana-api/no-warn
              (ana-api/analyze
               (ana-api/empty-env)
               `(apply
                 (fn [~@tag-syms ~@attr-vals-syms]
                   (h ~form))
                 [~@tag-vals ~@attr-vals-vals])
               {:optimizations :simple}))))
          :component
          (comp-api/emit
           (ana-api/no-warn
            (ana-api/analyze
             (assoc (ana-api/empty-env) :context :expr)
             `(fn [~@tag-syms ~@attr-vals-syms]
                (h ~form))
             {:optimizations :simple})))})
       (make-form-gen tag-gen)))))

(def valid-form-prop
  (prop/for-all [{:keys [form string-clj string-cljs component]}
                 form-gen]
                (spec/valid? ::comp/form form)))

(comment

  (gen/sample tag-gen)
  (gen/sample attrs-gen)
  (gen/sample child-gen)
  (gen/sample html-tag-gen)
  (last (map :string-clj (gen/sample form-gen 10)))
  (last (map :component (gen/sample form-gen 10)))
  (last (map :string-cljs (gen/sample form-gen 10)))

  (tc/quick-check 10 valid-form-prop)

  (loop [x (map :string-clj (gen/sample form-gen 10))]
    (if-let [f (first x)]
      (do
        (prn f)
        (eval f)
        (recur (rest x)))
      nil))
  )
