(ns ewen.inccup.incremental.core-test
  (:require [cljs.test :refer-macros [deftest testing is run-tests]]
            [ewen.inccup.core :refer-macros [html defhtml]]))

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

(comment
  (run-tests 'ewen.inccup.incremental.core-test)
  )
