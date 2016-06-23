(ns ewen.inccup.common.gen
  (:require [ewen.inccup.common.utils-test :as utils]
            [ewen.inccup.compiler :as comp :refer [h]]
            [ewen.inccup.common.spec]
            [clojure.spec :as spec]
            [clojure.test :refer [is]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [cljs.analyzer.api :as ana-api]
            [cljs.compiler.api :as comp-api]
            [cljs.repl]
            [clojure.data.xml :as xml]))

(def downcase-char-alpha-gen
  (gen/fmap char
            (gen/one-of [(gen/choose 97 122)])))

(def downcase-char-alphanumeric-gen
  (gen/fmap char
            (gen/one-of [(gen/choose 48 57)
                         (gen/choose 97 122)])))

(def tag-name-gen
  "Generator for keywords starting with an downcase alphanumeric char,
  other chars are downcase alphanumeric"
  (gen/fmap (fn [[f-char r-chars]]
              (-> (into [f-char] r-chars) clojure.string/join keyword))
            (gen/tuple downcase-char-alpha-gen
                       (gen/vector downcase-char-alphanumeric-gen))))

(def attr-name-gen
  "Generator for keywords starting with an alphanumeric char, other chars
  are alphanumeric or \\-"
  (gen/fmap (fn [[f-char r-chars]]
              (-> (into [f-char] r-chars) clojure.string/join keyword))
            (gen/tuple downcase-char-alpha-gen
                       (gen/vector
                        (gen/one-of [downcase-char-alphanumeric-gen
                                     (gen/return\-)])))))

(def keyword-gen
  "Generator for namespaced keywords"
  (gen/one-of [gen/keyword gen/keyword-ns]))

(def html-tag-gen
  (gen/elements
   #{:a :abbr :address :area :article :aside :audio :b :base :bdi :bdo
     :big :blockquote :br :button :canvas :cite :code
     :data :datalist :dd :del :details :dfn :dialog :div :dl :dt
     :em :embed :fieldset :figcaption :figure :footer :form :h1 :h2 :h3 :h4
     :h5 :h6 :header :hgroup :hr :i :img :input :ins
     :kbd :keygen :label :legend :li :link :main :map :mark :menu :menuitem
     :meta :meter :nav :noscript :object :ol :optgroup :option :output :p
     :param :picture :pre :progress :q :rp :rt :ruby :s :samp :script
     :section :small :source :span :strong :sub :summary :sup
     :time :track :u :ul :var :video :wbr}))

;;TODO handle textarea
;;TODO handle iframe
;;TODO handle select
;;TODO handle table

(def html-tags-with-context
  #{:tfoot :thead :th :html :head :td :title :colgroup :caption :col :tr
    :tbody :body :style :textarea :iframe :select :table})

(defn make-tag-gen
  "Generator for literal tags are a symbol amongs the one provided.
  The symbols represent parametrized tags"
  [tag-syms]
  (->> (if (empty? tag-syms)
         (gen/one-of [tag-name-gen html-tag-gen])
         (gen/one-of [tag-name-gen html-tag-gen
                      (gen/elements tag-syms)]))
       (gen/such-that #(not (contains? html-tags-with-context %)))))

(def attrs-gen (gen/map attr-name-gen gen/string-ascii))
(def child-gen (gen/one-of [gen/string-ascii (gen/return nil)]))
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
           {:max-elements 5
            :min-elements 1}))

(def attr-vals-gen
  (gen/map syms-gen (gen/vector gen/string-ascii 1 20)
           {:max-elements 5
            :min-elements 1}))

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
          #_:string-fn-cljs
          #_(binding [comp/*cljs-output-mode* :string]
            (comp-api/emit
             (ana-api/no-warn
              (ana-api/analyze
               (ana-api/empty-env)
               `(apply
                 (fn [~@tag-syms ~@attr-vals-syms]
                   (h ~form))
                 [~@tag-vals ~@attr-vals-vals])
               {:optimizations :simple}))))
          #_:component
          #_(comp-api/emit
           (ana-api/no-warn
            (ana-api/analyze
             (assoc (ana-api/empty-env) :context :expr)
             `(fn [~@tag-syms ~@attr-vals-syms]
                (h ~form))
             {:optimizations :simple})))})
       (make-form-gen tag-gen)))))

(defmacro with-eval [& body]
  `(utils/eval-js
    repl-env
    (utils/compile-cljs ~@body)))

(defn make-valid-form-prop [repl-env]
  (prop/for-all
   [{form :form {tags :tags attr-vals :attr-vals :as params} :params}
    form-gen]
   (spec/valid? ::comp/form form)
   (let [tag-syms (keys tags)
         attr-vals-syms (keys attr-vals)
         tag-vals (vals tags)
         attr-vals-vals (vals attr-vals)
         string-clj (-> `(apply (fn [~@tag-syms ~@attr-vals-syms]
                                  (h ~form))
                                [~@(map first tag-vals)
                                 ~@(map first attr-vals-vals)])
                        eval str)]

     ;; Def the string templating fn
     (binding [comp/*cljs-output-mode* :string]
       (with-eval
         (utils/cljs-test-quote
          (set! gen-client/string-fn-cljs
                (fn [~@tag-syms ~@attr-vals-syms]
                  (comp/h ~form))))))

     ;; Def the component fn
     (with-redefs [comp/cljs-env? (constantly true)]
       (with-eval
         (utils/cljs-test-quote
          (set! gen-client/component-fn
                (fn [~@tag-syms ~@attr-vals-syms]
                  (comp/h ~form))))))

     ;; Check that the clojure generated string is equal to the cljs
     ;; generated string for initial params
     (is (= string-clj
            (with-eval
              (utils/cljs-test-quote
               (c/str (c/apply gen-client/string-fn-cljs
                               [~@(map first tag-vals)
                                ~@(map first attr-vals-vals)]))))))

     ;; Render the clojurescript component for initial params
     (with-eval
       (utils/cljs-test-quote
        (set! gen-client/component
              (vdom/render! (utils/new-root) gen-client/component-fn
                            ~@(map first tag-vals)
                            ~@(map first attr-vals-vals)))))

     ;; Check that the clojure generated string is equal to the rendered
     ;; component
     (let [string-inc (with-eval
                        (utils/cljs-test-quote
                         (utils/node-to-string (utils/root))))]
       (is (= (xml/parse (java.io.StringReader. string-clj))
              (xml/parse (java.io.StringReader. string-inc)))))

     (loop [tag-vals-i 0
            attr-vals-vals-i 0
            i 0
            l (max (count tag-vals) (count attr-vals-vals))]
       (when (< i l)
         (let [tag-vals (map #(get % tag-vals-i) tag-vals)
               attr-vals-vals (map #(get % attr-vals-vals-i)
                                   attr-vals-vals)
               ;; Update the clojure string with the new params
               string-clj (-> `(apply (fn [~@tag-syms ~@attr-vals-syms]
                                        (h ~form))
                                      [~@tag-vals
                                       ~@attr-vals-vals])
                              eval str)])

         ;; Update the clojurescript component
         (with-eval
           (utils/cljs-test-quote
            (vdom/update! gen-client/component gen-client/component-fn
                          ~@tag-vals
                          ~@attr-vals-vals)))

         ;; Check that the clojure generated string is equal to the
         ;; rendered component
         (let [string-inc (with-eval
                            (utils/cljs-test-quote
                             (utils/node-to-string (utils/root))))]
           #_(is (= (xml/parse (java.io.StringReader. string-clj))
                    (xml/parse (java.io.StringReader. string-inc)))))

         (recur (if (>= tag-vals-i (dec (count tag-vals)))
                  0
                  (inc tag-vals-i))
                (if (>= attr-vals-vals-i (dec (count attr-vals-vals)))
                  0
                  (inc attr-vals-vals-i))
                (inc i)
                l)))

     true)))

(comment

  (gen/sample tag-gen)
  (gen/sample attrs-gen)
  (gen/sample child-gen)
  (gen/sample html-tag-gen)
  (gen/sample form-gen 10)

  (require '[ewen.replique.server-cljs :refer [repl-env]])
  (tc/quick-check 10 (gen/no-shrink (make-valid-form-prop repl-env)))
  )
