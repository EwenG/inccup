(ns ewen.inccup.core
  (:require [ewen.inccup.compiler-string-macros
             :refer [compile-html compile-html*]]
            [ewen.inccup.compiler-data-macros
             :refer [compile-data]]
            [ewen.inccup.util
             :refer [default-output-format *html-mode*
                     *is-top-level* *pre-compile* *output-format*]]))

(defn maybe-convert-raw-string [compile-fn content]
  `(let [out-str# (binding [*is-top-level* false]
                    ~(apply compile-fn content))]
     (if *is-top-level*
       (str out-str#) out-str#)))

(defmacro html
  [options & content]
  (let [mode (and (map? options) (:mode options))
        pre-compile-opt (when (map? options) (:pre-compile options))
        pre-compile? (if (nil? pre-compile-opt)
                       *pre-compile* pre-compile-opt)
        output-format (and (map? options) (:output-format options))
        output-format (or output-format *output-format*)
        output-string? (= :string
                          (or output-format (default-output-format &env)))
        content (if (or mode pre-compile-opt output-format)
                  (first content) options)]
    (cond (not output-string?)
          (compile-data content)
          (and pre-compile? mode)
          (binding [*html-mode* (or mode *html-mode*)]
            `(binding [*html-mode* (or ~mode *html-mode*)]
               ~(maybe-convert-raw-string compile-html content)))
          pre-compile?
          (maybe-convert-raw-string compile-html content)
          mode
          `(binding [*html-mode* (or ~mode *html-mode*)]
             ~(maybe-convert-raw-string compile-html* content))
          :else
          (maybe-convert-raw-string compile-html* content))))
