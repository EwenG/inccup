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

(def attrs-gen (gen/map attr-name-gen gen/string-ascii))
(def child-gen (gen/one-of [gen/string-ascii (gen/return nil)]))

(defn make-child-seq-gen [child-gen])
(def child-seq-gen (gen/fmap (fn [children] `(list ~@children))
                             (gen/list child-gen)))

(comment
  (gen/sample child-seq-gen)

  (map :form (gen/sample form-gen))
  )

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
           ;; Generates 10 values for each symbol
           (gen/vector tag-name-gen 10 10)
           ;; Number of tag symbols parameters
           {:max-elements 5}))

(def attr-vals-gen
  (gen/map syms-gen (gen/vector gen/string-ascii 10 10)
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
          :form form})
       (make-form-gen tag-gen)))))

(defmacro with-eval [repl-env & body]
  `(utils/eval-js
    ~repl-env
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
         repl-env
         (utils/cljs-test-quote
          (set! gen-client/string-fn-cljs
                (fn [~@tag-syms ~@attr-vals-syms]
                  (comp/h ~form))))))

     ;; Def the component fn
     (with-redefs [comp/cljs-env? (constantly true)]
       (with-eval
         repl-env
         (utils/cljs-test-quote
          (set! gen-client/component-fn
                (fn [~@tag-syms ~@attr-vals-syms]
                  (comp/h ~form))))))

     ;; Check that the clojure generated string is equal to the cljs
     ;; generated string for initial params
     (is (= string-clj
            (with-eval
              repl-env
              (utils/cljs-test-quote
               (c/str (c/apply gen-client/string-fn-cljs
                               [~@(map first tag-vals)
                                ~@(map first attr-vals-vals)]))))))

     ;; Render the clojurescript component for initial params
     (with-eval
       repl-env
       (utils/cljs-test-quote
        (set! gen-client/component
              (vdom/render! (utils/new-root) gen-client/component-fn
                            ~@(map first tag-vals)
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
            l 2]
       (when (< i l)
         (let [tag-vals (map #(get % i) tag-vals)
               attr-vals-vals (map #(get % i) attr-vals-vals)
               ;; Update the clojure string with the new params
               string-clj (-> `(apply (fn [~@tag-syms ~@attr-vals-syms]
                                        (h ~form))
                                      [~@tag-vals
                                       ~@attr-vals-vals])
                              eval str)]

         ;; Update the clojurescript component
         (with-eval
           repl-env
           (utils/cljs-test-quote
            (vdom/update! gen-client/component gen-client/component-fn
                          ~@tag-vals
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

(comment

  (gen/sample tag-gen)
  (gen/sample attrs-gen)
  (gen/sample child-gen)
  (gen/sample html-tag-gen)
  (gen/sample form-gen 10)

  (with-eval
    ewen.replique.server-cljs/repl-env
    (utils/cljs-test-quote
     (~'ns ~'cljs.user
      (:require [ewen.inccup.common.gen-client]))))

  (require '[ewen.replique.server-cljs :refer [repl-env]])

  (tc/quick-check
   80 (gen/no-shrink
        (make-valid-form-prop ewen.replique.server-cljs/repl-env)))
  )
