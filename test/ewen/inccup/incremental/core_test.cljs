(ns ewen.inccup.incremental.core-test
  (:require [cljs.test :refer-macros [deftest testing is run-tests]]
            [ewen.inccup.core :refer-macros [html defhtml]]
            [ewen.inccup.incremental.compiler :refer [*cache* init-cache]]
            [cljs.pprint :refer [pprint] :refer-macros [pp]]))

(set-print-fn! #(.log js/console %))

(defhtml def1 [x] [:div#ii.cc {} x])
(defhtml def2 [x y z] [:div#ii.cc x y z])
(defhtml def3 [x y z] [x y z])

(deftest test-defhtml
  (testing "defhtml"
    (binding [*cache* (init-cache)]
      (is (= (def1 "e")
             ["div" {:id "ii" :class "cc"} "e"]))
      (is (= (def1 [:p])
             ["div" {:id "ii" :class "cc"} [:p]])))
    (binding [*cache* (init-cache)]
      (is (= (def2 {:e "e"} [:p] "r")
             ["div" {:id "ii", :class "cc", :e "e"} nil [:p] "r"]))
      (is (= (def2 [:div] [:p] 3)
             ["div" {:id "ii", :class "cc"} [:div] [:p] 3])))
    (binding [*cache* (init-cache)]
      (is (= (def3 'p#ii.cc {:e "e"} "t")
             ["p" {:id "ii", :class "cc", :e "e"} nil "t"]))
      (is (= (def3 'div {:f "f"} "t")
             ["div" {:f "f"} nil "t"]))
      (is (= (def3 'div.e [:p] "t")
             ["div" {:class "e"} [:p] "t"])))))

(def cache-seq (atom []))
(defhtml template1 [x] [:p {} x])
(defhtml template2 [x y] [:p {} x (html [:p x]) (template1 y)])
(defhtml template3 [x] [:p {} (count x) (for [y x] (template1 y))])

(deftest test-cache
  (testing "cache"
    (binding [*cache* (init-cache)]
      (reset! cache-seq [])
      (let [res1 (template2 1 2)
            _ (swap! cache-seq conj (js->clj *cache*))
            res2 (template2 3 4)
            _ (swap! cache-seq conj (js->clj *cache*))
            res3 (template2 3 4)]
        (is (not= res1 res3))
        (is (identical? res2 res3))
        (is (identical? (get-in res1 [4 1]) (get-in res2 [4 1])))
        @cache-seq
        (is
         (=
          @cache-seq
          '[{"dynamic-counter" 1,
             "dynamic-array"
             [{"sub-cache"
               [{"dynamic-counter" 1,
                 "dynamic-array" [{"sub-cache" [],
                                   "prev-result" ["p" {} 1]}]}
                {"dynamic-counter" 1,
                 "dynamic-array"
                 [{"sub-cache" [],
                   "params" [2],
                   "prev-result" ["p" {} 2]}]}],
               "params" [1 2],
               "prev-result" ["p" {} 1 ["p" {} 1] ["p" {} 2]]}]}
            {"dynamic-counter" 1,
             "dynamic-array"
             [{"sub-cache"
               [{"dynamic-counter" 1,
                 "dynamic-array"
                 [{"sub-cache" [],
                   "prev-result" ["p" {} 3],
                   "params" nil}]}
                {"dynamic-counter" 1,
                 "dynamic-array"
                 [{"sub-cache" [],
                   "prev-result" ["p" {} 4],
                   "params" [4]}]}],
               "prev-result" ["p" {} 3 ["p" {} 3] ["p" {} 4]],
               "params" [3 4]}]}])))))

  #_(testing "dynamic-cache"
      (binding [*cache* (init-cache)]
        (reset! cache-seq [])
        (let [res1 (template3 [1 2])
              _ (prn res1)
              _ (swap! cache-seq conj (js->clj *cache*))
              res2 (template3 [1 2])
              _ (prn res2)
              res3 (template3 [1 2 3])
              _ (prn res3)
              _ (swap! cache-seq conj (js->clj *cache*))
              res4 (template3 [6])
              _ (prn res4)
              _ (swap! cache-seq conj (js->clj *cache*))]
          (identical? res1 res2)
          (is (not= res1 res3 res4))
          (prn @cache-seq)))))





(comment
  (run-tests 'ewen.inccup.incremental.core-test)
  )

(comment

  (binding [*cache* (init-cache)]
    (let [res1 (template3 [1])]
      (prn *cache*)))
  )
