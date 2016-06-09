(ns ewen.inccup.compiler
  (:require [ewen.inccup.string.compiler :refer [compile-string]]
            [ewen.inccup.incremental.compiler :refer [component]]
            [ewen.inccup.common.util :refer [name-with-attributes]]
            [ewen.inccup.common.runtime :as c-runtime]
            [clojure.spec :as spec]
            [cljs.tagged-literals :refer [*cljs-data-readers*]]))

;; Options specs
(spec/def ::string-output-mode #(= :string %))
(spec/def ::output-mode #{:string :incremental})
(spec/def ::options (spec/keys :opt [::output-mode]))

(spec/fdef comp/h
           :args (spec/alt :no-option ::spec/any
                           :with-options (spec/cat
                                          :forms ::spec/any
                                          :options ::options)))

(def ^:dynamic *clj-output-mode* :string)
;; One must use a side effectful macro in order to modify the root value
;; from clojurescript
(def ^:dynamic *cljs-output-mode* :incremental)

(defn cljs-env?
  "Take the &env from a macro, and tell whether we are expanding
  into cljs."
  [env]
  (boolean (:ns env)))

#_(defn options-with-content [options-content env]
  (let [[options content] (if (map? (first options-content))
                              [(first options-content)
                               (rest options-content)]
                              [{} options-content])
        mode (:mode options)
        output-format (or (:output-format options)
                          *output-format*
                          (default-output-format env))]
    (assert (= 1 (count content))
            "Inccup must be given a single expression")
    (assert (or (= :string output-format)
                (and (cljs-env? env) (= :inccup output-format)))
            "Invalid output format")
    [{:mode mode :output-format output-format} (first content)]))

(defn map->js-obj [m]
  (loop [m m
         data []]
    (if-let [[k v] (first m)]
      (let [v (if (map? v) (map->js-obj v) v)]
        (recur (rest m) (conj data (name k) v)))
      `(cljs.core/js-obj ~@data))))

(defmacro with-opts [{:keys [key level] :or {level 1} :as opts} comp]
  (when level (assert (not (nil? key))))
  (assert (or (nil? level) (and (number? level) (> level 0))))
  `(ewen.inccup.incremental.vdom/set-opts ~comp ~(map->js-obj opts)))

(defn compile-dispatch-cljs
  ([env forms]
   (compile-dispatch-cljs env forms {::output-mode *cljs-output-mode*}))
  ([env forms opts]
   {:pre [(vector? forms)]}
   (let [opts (if-not (contains? opts ::output-mode)
                (assoc opts ::output-mode *cljs-output-mode*)
                opts)
         {:keys [::output-mode] :as opts} (spec/conform ::options opts)]
     (assert (not= ::spec/invalid opts))
     (case output-mode
       :string (compile-string forms)
       :incremental (component env forms)))))

(defn compile-dispatch-clj
  ([forms]
   (compile-dispatch-clj forms {::output-mode *clj-output-mode*}))
  ([forms {:keys [::output-mode] :or {::output-mode *clj-output-mode*}}]
   {:pre [(vector? forms)
          (spec/valid? ::string-output-mode output-mode)]}
   `(binding [c-runtime/*attrs-or-first-child* nil]
      ~(compile-string forms))))

(defmacro h
  ([forms]
   (if (cljs-env? &env)
     (compile-dispatch-cljs &env forms)
     (compile-dispatch-clj forms)))
  ([forms opts]
   (if (cljs-env? &env)
     (compile-dispatch-cljs &env forms opts)
     (compile-dispatch-clj forms opts))))

(defn tagged-literal->h [form]
  `(h ~form))

(defmacro register-tagged-literal! [sym]
  (alter-var-root #'*cljs-data-readers* assoc sym tagged-literal->h)
  (alter-var-root #'*data-readers* assoc sym tagged-literal->h)
  (set! *data-readers* (assoc *data-readers* sym tagged-literal->h))
  `'~sym)

(comment
  (register-tagged-literal! h)
  )

#_(defmacro html
  [& options-content]
  (let [[{:keys [mode output-format]} content] (options-with-content
                                                options-content &env)]
    (cond (= :inccup output-format)
          `(compile-inc ~content)
          mode
          (binding [*html-mode* (or mode *html-mode*)]
            `(binding [*html-mode* (or ~mode *html-mode*)]
               ~(maybe-convert-raw-string compile-html content)))
          :else
          (maybe-convert-raw-string compile-html content))))

#_(defmacro defhtml
  "Define a function, but wrap its output in an implicit html macro."
  [name & meta-body]
  (let [[name [args & options-content]] (name-with-attributes
                                         name meta-body)
        name (vary-meta name assoc ::defhtml true)
        [{:keys [mode output-format]} content] (options-with-content
                                                options-content &env)]
    (cond (= :inccup output-format)
          (let [params (extract-params args)
                update-fn-sym (gensym "update-fn")
                static-sym (gensym "static")]
            `(let ~(compile-inc-with-params
                    &env content params update-fn-sym static-sym)
               (defn ~name ~args
                 (ewen.inccup.incremental.compiler/Component.
                  ~static-sym false ~update-fn-sym
                  (cljs.core/array ~@params) ~(count params)
                  nil))))
          mode
          (binding [*html-mode* (or mode *html-mode*)]
            `(binding [*html-mode* (or ~mode *html-mode*)]
               (defn ~name ~args
                 ~(maybe-convert-raw-string compile-html content))))
          :else
          `(defn ~name ~args
             ~(maybe-convert-raw-string compile-html content)))))
