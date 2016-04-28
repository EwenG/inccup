(ns ewen.inccup.incremental.core-test
  (:require [cljs.test :refer-macros [deftest testing is run-tests]]
            [ewen.inccup.core :refer-macros [html defhtml]]
            [ewen.inccup.incremental.compiler :refer
             [*cache* init-cache clean-cache]]
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
            '[{"sub-cache"
               [{"dynamic-counter" 1,
                 "dynamic-array" [{"sub-cache" [],
                                   "version" 1,
                                   "prev-result" ["p" {} 1]}]}
                {"dynamic-counter" 1,
                 "dynamic-array"
                 [{"sub-cache" [],
                   "version" 1,
                   "params" [2],
                   "prev-result" ["p" {} 2]}]}],
               "version" 1,
               "params" [1 2],
               "prev-result" ["p" {} 1 ["p" {} 1] ["p" {} 2]]}
              {"sub-cache"
               [{"dynamic-counter" 1,
                 "dynamic-array"
                 [{"sub-cache" [],
                   "version" 2,
                   "prev-result" ["p" {} 3]}]}
                {"dynamic-counter" 1,
                 "dynamic-array"
                 [{"sub-cache" [],
                   "version" 2,
                   "prev-result" ["p" {} 4],
                   "params" [4]}]}],
               "version" 2,
               "prev-result" ["p" {} 3 ["p" {} 3] ["p" {} 4]],
               "params" [3 4]}])))))

  (testing "dynamic-cache"
      (binding [*cache* (init-cache)]
        (reset! cache-seq [])
        (let [res1 (template3 [1 2])
              _ (prn res1)
              _ (clean-cache *cache*)
              _ (swap! cache-seq conj (js->clj *cache*))
              res2 (template3 [1 2])
              _ (prn res2)
              _ (clean-cache *cache*)
              res3 (template3 [1 2 3])
              _ (prn res3)
              _ (clean-cache *cache*)
              _ (swap! cache-seq conj (js->clj *cache*))
              res4 (template3 [6])
              _ (prn res4)
              _ (clean-cache *cache*)
              _ (swap! cache-seq conj (js->clj *cache*))]
          (identical? res1 res2)
          (is (not= res1 res3 res4))
          (is
           (= @cache-seq
              '[{"sub-cache"
                 [{"dynamic-counter" 0, "dynamic-array" []}
                  {"dynamic-counter" 2,
                   "dynamic-array"
                   [{"sub-cache" [],
                     "version" 1,
                     "params" [1],
                     "prev-result" ["p" {} 1]}
                    {"sub-cache" [],
                     "version" 1,
                     "params" [2],
                     "prev-result" ["p" {} 2]}]}],
                 "version" 1,
                 "params" [[1 2]],
                 "prev-result" ["p" {} 2 (["p" {} 1] ["p" {} 2])]}
                {"sub-cache"
                 [{"dynamic-counter" 0, "dynamic-array" []}
                  {"dynamic-counter" 3,
                   "dynamic-array"
                   [{"sub-cache" [],
                     "version" 3,
                     "params" [1],
                     "prev-result" ["p" {} 1]}
                    {"sub-cache" [],
                     "version" 3,
                     "params" [2],
                     "prev-result" ["p" {} 2]}
                    {"sub-cache" [],
                     "version" 3,
                     "params" [3],
                     "prev-result" ["p" {} 3]}]}],
                 "version" 3,
                 "params" [[1 2 3]],
                 "prev-result"
                 ["p" {} 3 (["p" {} 1] ["p" {} 2] ["p" {} 3])]}
                {"sub-cache"
                 [{"dynamic-counter" 0, "dynamic-array" []}
                  {"dynamic-counter" 1,
                   "dynamic-array"
                   [{"sub-cache" [],
                     "version" 4,
                     "params" [6],
                     "prev-result" ["p" {} 6]}]}],
                 "version" 4,
                 "params" [[6]],
                 "prev-result" ["p" {} 1 (["p" {} 6])]}]))))))




(comment
  (run-tests 'ewen.inccup.incremental.core-test)

  (defhtml template2 [x y] [:p {} ^{:ewen.inccup.core/id 1} (template1 y)])
  )

(comment

  (defhtml template1 [x] [:p {} x])
  (defhtml template3 [x] [:p {} (count x) (for [y x] (template1 y))])

  (binding [*cache* (init-cache)]

    (let [res1 (template3 [1 2])
          _ (prn res1)
          res2 (template3 [1 2])
          _ (prn res2)
          res3 (template3 [1 2 3])
          _ (prn res3)
          res4 (template3 [6])
          _ (prn res4)]
      res4
      (clean-cache *cache*)
      (js->clj *cache*)))

  )
