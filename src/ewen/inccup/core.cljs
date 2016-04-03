(ns ewen.inccup.core
  (:require ewen.inccup.string.compiler
            ewen.inccup.incremental.compiler))

(comment
  (require '[ewen.inccup.core :refer-macros [html defhtml]])
  (require '[ewen.inccup.incremental.compiler
             :refer [*cache* init-cache clean-dynamic-array]])
  (require '[cljs.pprint :refer [pprint] :refer-macros [pp]])

  (defhtml tt [x] [:e.rr {} [:p {} x]])
  (defhtml tt [x] [:e.rr {} [:p.b x]])
  (defhtml tt [x y] [:e.rr {} [:p.a {:class x} y]])

  (defhtml tt [x] [:p {} x (html [:p])])

  (defhtml tt2 [x] [:p {} x])
  (defhtml tt [x] [:p {} x (tt2 3)])

  (binding [*cache* (init-cache)]
    (tt 4)
    (clean-dynamic-array *cache*)
    (.log js/console (str (js->clj *cache*)))
    (let [res (tt 5)]
      (clean-dynamic-array *cache*)
      res)
    (js->clj *cache*))
  )
