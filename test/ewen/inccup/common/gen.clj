(ns ewen.inccup.common.gen
  (:require [ewen.inccup.compiler :as comp :refer [h]]
            [ewen.inccup.common.spec]
            [clojure.spec :as spec]
            [clojure.test :refer [is]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [cljs.analyzer.api :as ana-api]
            [cljs.compiler.api :as comp-api]
            [cljs.repl]))

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

(defn make-tag-gen [tag-syms]
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
           {:max-elements 5}))

(def attr-vals-gen
  (gen/map syms-gen (gen/vector gen/string-ascii 1 20)
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

(defn eval-js [repl-env js]
  (let [{:keys [status value]}
        (cljs.repl/-evaluate repl-env "<cljs repl>" 1 js)]
    (is (= :success status))
    value))

(defn compile-cljs [cljs-form]
  (binding [cljs.analyzer/*cljs-ns* 'ewen.inccup.common.gen-client]
    (comp-api/emit
     (ana-api/no-warn
      (ana-api/analyze
       (assoc (ana-api/empty-env) :context :expr)
       cljs-form
       {:optimizations :simple})))))

(defn gen-client-sym [sym]
  (symbol "ewen.inccup.common.gen-client" (str sym)))

(def gen-client-ns "ewen.inccup.common.gen-client")
(def vdom-ns "ewen.inccup.incremental.vdom")

(defn make-valid-form-prop [repl-env]
  (prop/for-all
   [{form :form {tags :tags attr-vals :attr-vals :as params} :params}
    form-gen]
   (spec/valid? ::comp/form form)
   (let [tag-syms (keys tags)
         attr-vals-syms (keys attr-vals)
         tag-vals (vals tags)
         attr-vals-vals (vals attr-vals)
         compile-fn `(~'fn ~'x [~@tag-syms ~@attr-vals-syms]
                      (h ~form))
         string-clj (-> `(apply ~compile-fn
                                [~@(map first tag-vals)
                                 ~@(map first attr-vals-vals)])
                        eval str)]

     #_(eval-js repl-env
                (compile-cljs
                 `(def ~(gen-client-sym 'params) (quote ~params))))

     ;; Def the string templating fn
     (eval-js repl-env
              (binding [comp/*cljs-output-mode* :string]
                (compile-cljs
                 `(set! ~(gen-client-sym 'string-fn-cljs) ~compile-fn))))

     ;; Def the component fn
     (eval-js repl-env
              (with-redefs [comp/cljs-env? (constantly true)]
                (compile-cljs
                 `(set! ~(gen-client-sym 'component) ~compile-fn))))

     ;; Check that the clojure generated string is equal to the cljs
     ;; generated string for initial params
     (is (= string-clj
            (eval-js repl-env
                     (compile-cljs
                      `(apply ~(gen-client-sym 'string-fn-cljs)
                              [~@(map first tag-vals)
                               ~@(map first attr-vals-vals)])))))

     ;; Render the clojure generated string for initial params
     (eval-js repl-env
              (compile-cljs
               `(goog.object/set (~(symbol gen-client-ns "new-root-string"))
                               "innerHTML" ~string-clj)))

     ;; Render the clojurescript component for initial params
     (eval-js repl-env
              (compile-cljs
               `(~(symbol vdom-ns "render!")
                 (~(symbol gen-client-ns "new-root-comp"))
                 ~(symbol gen-client-ns "component")
                 ~@(map first tag-vals)
                 ~@(map first attr-vals-vals))))


     (= "true"
        (eval-js repl-env
                 (compile-cljs
                  `(~(symbol gen-client-ns "roots-equal?")))))
     #_(is (= "true"
            (eval-js repl-env
                     (compile-cljs
                      `(~(symbol gen-client-ns "roots-equal?"))))))
     #_true)))

(comment

  (gen/sample tag-gen)
  (gen/sample attrs-gen)
  (gen/sample child-gen)
  (gen/sample html-tag-gen)
  (gen/sample form-gen 10)

  (require '[ewen.replique.server-cljs :refer [repl-env]])
  (tc/quick-check 30 (gen/no-shrink (make-valid-form-prop repl-env)))
  )
