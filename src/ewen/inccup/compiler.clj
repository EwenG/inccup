(ns ewen.inccup.compiler
  (:require [ewen.inccup.string.compiler :refer [compile-string]]
            [ewen.inccup.incremental.compiler :refer [component]]
            [ewen.inccup.util
             :refer [name-with-attributes cljs-env?
                     default-output-format
                     *output-mode* *html-mode* *base-url*]]
            [cljs.tagged-literals :refer [*cljs-data-readers*]]))

(defn default-output-mode [env]
  (if (cljs-env? env) :incremental :string))

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

(defn tag-reader-fn-cljs
  ([forms]
   (tag-reader-fn-cljs forms nil))
  ([forms {:keys [html-mode output-mode]}]
   {:pre [(or (nil? output-mode)
              (= :string output-mode)
              (= :incremental output-mode))
          (or (nil? html-mode)
              (= :xhtml html-mode)
              (= :html html-mode)
              (= :sgml html-mode)
              (= :xml html-mode))
          (vector? forms)]}
   (if (or (= :string output-mode)
           (= :string *output-mode*))
     (binding [*html-mode* (or html-mode *html-mode*)]
       `(binding [*html-mode* (or ~html-mode *html-mode*)]
          (compile-string ~forms)))
     `(component ~forms))))

(defn tag-reader-fn-clj
  ([forms]
   (tag-reader-fn-clj forms nil))
  ([forms {:keys [html-mode]}]
   {:pre [(or (nil? html-mode)
              (= :xhtml html-mode)
              (= :html html-mode)
              (= :sgml html-mode)
              (= :xml html-mode))
          (vector? forms)]}
   (binding [*html-mode* (or html-mode *html-mode*)]
     `(binding [*html-mode* (or ~html-mode *html-mode*)]
        (compile-string ~forms)))))

(alter-var-root #'*cljs-data-readers* assoc 'h tag-reader-fn-cljs)

(alter-var-root #'*data-readers* assoc 'h tag-reader-fn-clj)
(set! *data-readers* (assoc *data-readers* 'h tag-reader-fn-clj))

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
