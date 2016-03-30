(ns ewen.inccup.core
  (:require ewen.inccup.string.compiler
            ewen.inccup.incremental.compiler))

(comment
  (require '[ewen.inccup.core :refer-macros [html defhtml]])
  (require '[cljs.pprint :refer [pprint] :refer-macros [pp]])

  (defhtml tt [x] [:e.rr {} [:p {} x]])
  (defhtml tt [x] [:e.rr {} [:p.b x]])
  (defhtml tt [x y] [:e.rr {} [:p.a {:class x} y]])
  )
