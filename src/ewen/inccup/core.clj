(ns ewen.inccup.core
  (:require [ewen.inccup.string.compiler-macros
             :refer [compile-html maybe-convert-raw-string]]
            [ewen.inccup.incremental.compiler-macros
             :refer [compile-inc extract-params]]
            [ewen.inccup.util
             :refer [default-output-format name-with-attributes cljs-env?
                     *html-mode* *output-format*]]))

(defn options-with-content [options-content env]
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

(defmacro html
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

(defmacro defhtml
  "Define a function, but wrap its output in an implicit html macro."
  [name & meta-body]
  (let [[name [args & options-content]] (name-with-attributes
                                         name meta-body)
        [{:keys [mode output-format]} content] (options-with-content
                                                options-content &env)]
    (cond (= :inccup output-format)
          (let [params (extract-params args)
                cache-sym (gensym "cache")]
            `(let [~cache-sym (atom {})]
               (defn ~name ~args
                 (compile-inc ~content ~params ~cache-sym))))
          mode
          (binding [*html-mode* (or mode *html-mode*)]
            `(binding [*html-mode* (or ~mode *html-mode*)]
               (defn ~name ~args
                 ~(maybe-convert-raw-string compile-html content))))
          :else
          `(defn ~name ~args
             ~(maybe-convert-raw-string compile-html content)))))
