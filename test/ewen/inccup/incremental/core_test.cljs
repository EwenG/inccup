(ns ewen.inccup.incremental.core-test
  (:require [cljs.test :refer-macros [deftest testing is run-tests]]
            [ewen.inccup.core :refer-macros [html defhtml]]
            [ewen.inccup.incremental.compiler
             :refer [*cache* init-cache clean-dynamic-array]]
            [cljs.pprint :refer [pprint] :refer-macros [pp]]))

(set-print-fn! #(.log js/console %))

(defhtml def1 [x] [:div#ii.cc {} x])
(defhtml def2 [x y z] [:div#ii.cc x y z])
(defhtml def3 [x y z] [x y z])

(deftest defhtml
  (testing "defhtml"
    (is (= (def1 "e")
           ["div" {:id "ii" :class "cc"} "e"]))
    (is (= (def1 [:p])
           ["div" {:id "ii" :class "cc"} [:p]]))
    (is (= (def2 {:e "e"} [:p] "r")
           ["div" {:id "ii", :class "cc", :e "e"} nil [:p] "r"]))
    (is (= (def2 [:div] [:p] 3)
           ["div" {:id "ii", :class "cc"} [:div] [:p] 3]))
    (is (= (def3 'p#ii.cc {:e "e"} "t")
           ["p" {:id "ii", :class "cc", :e "e"} nil "t"]))
    (is (= (def3 'div {:f "f"} "t")
           ["div" {:f "f"} nil "t"]))
    (is (= (def3 'div.e [:p] "t")
           ["div" {:class "e"} [:p] "t"]))))

(defhtml template2 [x] [:p {} x])
(defhtml template1 [x y] [:p {} x (html [:p x]) (template2 y)])
(def cache-seq (atom []))

(deftest cache
  (testing "cache"
    (reset! cache-seq [])
    (binding [*cache* (init-cache)]
      (template1 1 2)
      (clean-dynamic-array *cache*)
      (swap! cache-seq conj (js->clj *cache*))
      (template1 3 4)
      (clean-dynamic-array *cache*)
      (swap! cache-seq conj (js->clj *cache*)))
    (is
     (=
      @cache-seq
      '[{"dynamic-counter" 0
         "dynamic-array"
         [{"sub-cache"
           [{"dynamic-counter" 0
             "dynamic-array" [{"sub-cache" [] "prev-result" ["p" {} 1]}]}
            {"dynamic-counter" 0
             "dynamic-array"
             [{"sub-cache" [] "params" {x 2} "prev-result" ["p" {} 2]}]}]
           "params" {x 1 y 2}
           "prev-result" ["p" {} 1 ["p" {} 1] ["p" {} 2]]}]}
        {"dynamic-counter" 0
         "dynamic-array"
         [{"sub-cache"
           [{"dynamic-counter" 0
             "dynamic-array" [{"sub-cache" [] "prev-result" ["p" {} 3]}]}
            {"dynamic-counter" 0
             "dynamic-array"
             [{"sub-cache" [] "params" {x 4} "prev-result" ["p" {} 4]}]}]
           "params" {x 3 y 4}
           "prev-result" ["p" {} 3 ["p" {} 3] ["p" {} 4]]}]}]))))

(comment
  (run-tests 'ewen.inccup.incremental.core-test)
  )
