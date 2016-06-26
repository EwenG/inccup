(ns ewen.inccup.gen
  (:require [ewen.inccup.utils-test :as utils]
            [ewen.inccup.compiler :as comp :refer [h]]
            [ewen.inccup.common.spec]
            [clojure.spec :as spec]
            [clojure.test :refer [is]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.data.xml :as xml]))

(def downcase-char-alpha-gen
  (gen/fmap char (gen/one-of [(gen/choose 97 122)])))

(def downcase-char-alphanumeric-gen
  (gen/fmap char (gen/one-of [(gen/choose 48 57)
                              (gen/choose 97 122)])))

(def tag-name-gen
  "Generator for keywords starting with an downcase alpha char,
  other chars are downcase alphanumeric"
  (gen/fmap (fn [[f-char r-chars]]
              (-> (into [f-char] r-chars) clojure.string/join keyword))
            (gen/tuple downcase-char-alpha-gen
                       (gen/vector downcase-char-alphanumeric-gen))))

(def attr-name-gen
  "Generator for keywords starting with an alpha char, other chars
  are alphanumeric or \\-"
  (gen/fmap (fn [[f-char r-chars]]
              (-> (into [f-char] r-chars) clojure.string/join keyword))
            (gen/tuple downcase-char-alpha-gen
                       (gen/vector
                        (gen/one-of [downcase-char-alphanumeric-gen
                                     (gen/return\-)])))))

(def keyword-gen
  "Generator for namespaced or not namespaced keywords"
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
     :time :track :u :ul :var :video :wbr :tfoot :thead :th :td :title
     :colgroup :caption :col :tr :tbody :select :table :textarea :iframe
     :html :head  :body :style}))

(defn make-tag-gen
  "Generator for literal tags are a symbol amongs the ones provided.
  The symbols represent parametrized tags"
  [tag-syms]
  (if (empty? tag-syms)
    (gen/one-of [tag-name-gen html-tag-gen])
    (gen/one-of [tag-name-gen html-tag-gen
                 (gen/elements tag-syms)])))

(def literal-attrs-gen
  (gen/map attr-name-gen gen/string-ascii))

(defn make-attrs-gen
  "Generator for attributes. Generates h-maps of keywords generated with
  attr-name-gen as keys and ascii strings as values. Keys and values can
  also be symbols chosen among ones of the provided symbols. The map itself
  can also be a symbol instead of a map, the symbol is also chosen among
  the provided ones"
  [attr-syms attr-keys-syms attr-vals-syms]
  (let [not-sym-attrs-gen
        (gen/map (if (empty? attr-keys-syms)
                   attr-name-gen
                   (gen/one-of [attr-name-gen
                                (gen/elements attr-keys-syms)]))
                 (if (empty? attr-vals-syms)
                   gen/string-ascii
                   (gen/one-of [gen/string-ascii
                                (gen/elements attr-vals-syms)])))]
    (if (empty? attr-syms)
      not-sym-attrs-gen
      (gen/one-of [not-sym-attrs-gen (gen/elements attr-syms)]))))

(comment
  (gen/sample (make-attrs-gen #{'a 'b} #{'e 'f} #{'g 'h}))
  )

(def child-gen (gen/one-of [gen/string-ascii (gen/return nil)]))
(def child-seq-gen (gen/fmap (fn [children] `(list ~@children))
                             (gen/list child-gen)))

(comment
  (gen/sample child-seq-gen)
  )

(defn make-form-container-gen [tag-gen attrs-gen]
  (fn [child-gen]
    (gen/let [tag tag-gen
              attr attrs-gen
              children (gen/vector
                        (gen/one-of
                         [child-gen child-seq-gen]))]
      (gen/one-of [(gen/return (into [tag attr] children))
                   (gen/return (into [tag] children))]))))

(defn make-form-gen [tag-gen attrs-gen]
  (gen/recursive-gen (make-form-container-gen tag-gen attrs-gen)
                     child-gen))

(def string-alpha
  (gen/fmap clojure.string/join
            (gen/not-empty (gen/vector gen/char-alpha))))

(def syms-gen (gen/fmap symbol string-alpha))

(def tag-params-gen
  (gen/map syms-gen
           ;; Generates 10 values for each symbol
           (gen/vector tag-name-gen 10 10)
           ;; Number of tag symbols parameters
           {:max-elements 4}))

(def attr-params-gen
  (gen/map syms-gen (gen/vector literal-attrs-gen 10 10)
           {:max-elements 4}))

(def attr-keys-params-gen
  (gen/map syms-gen (gen/vector attr-name-gen 10 10)
           {:max-elements 4}))

(def attr-vals-params-gen
  (gen/map syms-gen (gen/vector gen/string-ascii 10 10)
           {:max-elements 4}))

(comment
  (gen/sample attr-keys-params-gen)
  )

(def params-gen (gen/let [tag-params tag-params-gen
                          attr-params attr-params-gen
                          attr-keys-params attr-keys-params-gen
                          attr-vals-params attr-vals-params-gen]
                  {:tags tag-params
                   :attrs attr-params
                   :attr-keys attr-keys-params
                   :attr-vals attr-vals-params}))

(comment
  (gen/sample params-gen)
  )

(def form-gen
  (gen/let [{:keys [tags attrs attr-keys attr-vals]
             :as params}
            params-gen]
    (let [tag-syms (keys tags)
          attr-syms (keys attrs)
          attr-keys-syms (keys attr-keys)
          attr-vals-syms (keys attr-vals)
          tag-gen (make-tag-gen tag-syms)
          attrs-gen (make-attrs-gen
                     attr-syms attr-keys-syms attr-vals-syms)]
      (gen/fmap
       (fn [form]
         {:params params
          :form form})
       (make-form-gen tag-gen attrs-gen)))))

(defmacro with-eval [repl-env & body]
  `(utils/eval-js
    ~repl-env
    (utils/compile-cljs ~@body)))

(defn make-valid-form-prop [repl-env]
  (prop/for-all
   [{form :form {tags-params :tags
                 attrs-params :attrs
                 attr-keys-params :attr-keys
                 attr-vals-params :attr-vals
                 :as params} :params}
    form-gen]
   (spec/valid? ::comp/form form)
   (let [tag-syms (keys tags-params)
         attr-syms (keys attrs-params)
         attr-keys-syms (keys attr-keys-params)
         attr-vals-syms (keys attr-vals-params)
         tag-vals (vals tags-params)
         attr-vals (vals attrs-params)
         attr-keys-vals (vals attr-keys-params)
         attr-vals-vals (vals attr-vals-params)
         string-clj (-> `(apply (fn [~@tag-syms ~@attr-syms
                                     ~@attr-keys-syms ~@attr-vals-syms]
                                  (h ~form))
                                [~@(map first tag-vals)
                                 ~@(map first attr-vals)
                                 ~@(map first attr-keys-vals)
                                 ~@(map first attr-vals-vals)])
                        eval str)]

     ;; Def the string templating fn
     (binding [comp/*cljs-output-mode* :string]
       (with-eval
         repl-env
         (utils/cljs-test-quote
          (set! gen-client/string-fn-cljs
                (fn [~@tag-syms ~@attr-syms
                     ~@attr-keys-syms ~@attr-vals-syms]
                  (comp/h ~form))))))

     ;; Def the component fn
     (with-redefs [comp/cljs-env? (constantly true)]
       (with-eval
         repl-env
         (utils/cljs-test-quote
          (set! gen-client/component-fn
                (fn [~@tag-syms ~@attr-syms
                     ~@attr-keys-syms ~@attr-vals-syms]
                  (comp/h ~form))))))

     ;; Check that the clojure generated string is equal to the cljs
     ;; generated string for initial params
     (is (= string-clj
            (with-eval
              repl-env
              (utils/cljs-test-quote
               (c/str (c/apply gen-client/string-fn-cljs
                               [~@(map first tag-vals)
                                ~@(map first attr-vals)
                                ~@(map first attr-keys-vals)
                                ~@(map first attr-vals-vals)]))))))

     ;; Render the clojurescript component for initial params
     (with-eval
       repl-env
       (utils/cljs-test-quote
        (set! gen-client/component
              (vdom/render! (utils/new-root) gen-client/component-fn
                            ~@(map first tag-vals)
                            ~@(map first attr-vals)
                            ~@(map first attr-keys-vals)
                            ~@(map first attr-vals-vals)))))

     ;; Check that the clojure generated string is equal to the rendered
     ;; component
     (let [string-inc (with-eval
                        repl-env
                        (utils/cljs-test-quote
                         (utils/node-to-string (utils/root))))]
       (is (= (xml/parse (java.io.StringReader. string-clj))
              (xml/parse (java.io.StringReader. string-inc)))))

     (loop [i 1
            l 4]
       (when (< i l)
         (let [tag-vals (map #(get % i) tag-vals)
               attr-vals-vals (map #(get % i) attr-vals-vals)
               ;; Update the clojure string with the new params
               string-clj (-> `(apply (fn [~@tag-syms ~@attr-syms
                                           ~@attr-keys-syms
                                           ~@attr-vals-syms]
                                        (h ~form))
                                      [~@tag-vals
                                       ~@attr-vals
                                       ~@attr-keys-vals
                                       ~@attr-vals-vals])
                              eval str)]

         ;; Update the clojurescript component
         (with-eval
           repl-env
           (utils/cljs-test-quote
            (vdom/update! gen-client/component
                          ~@tag-vals
                          ~@attr-vals
                          ~@attr-keys-vals
                          ~@attr-vals-vals)))

         ;; Check that the clojure generated string is equal to the
         ;; rendered component
         (let [string-inc (with-eval
                            repl-env
                            (utils/cljs-test-quote
                             (utils/node-to-string (utils/root))))]
           (is (= (xml/parse (java.io.StringReader. string-clj))
                  (xml/parse (java.io.StringReader. string-inc)))))

         (recur (inc i) l))))

     true)))

(defn run-cljs-tests [repl-env test-nb-runs]
  (tc/quick-check
   test-nb-runs (gen/no-shrink
                 (make-valid-form-prop repl-env))))

(comment

  (gen/sample child-gen)
  (gen/sample html-tag-gen)
  (gen/sample form-gen 10)
  (map :form (gen/sample form-gen))

  (require '[ewen.replique.server-cljs :refer [repl-env]])

  (tc/quick-check 30 (gen/no-shrink (make-valid-form-prop repl-env)))
  )
