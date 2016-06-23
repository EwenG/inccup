(ns ewen.inccup.incremental.core-test
  (:require [cljs.test :refer-macros [deftest testing is run-tests]]
            [ewen.inccup.compiler
             :refer-macros [with-opts! register-tagged-literal! h]
             :as comp]
            [ewen.inccup.incremental.vdom :as vdom
             :refer [Component render! update!]]
            [ewen.inccup.common.utils-test
             :refer [node-to-string root new-root]]
            [cljs.pprint :refer [pprint] :refer-macros [pp]]
            [goog.array]
            [goog.dom])
  (:require-macros [ewen.inccup.incremental.core-test
                    :refer [multi-defn]]))

(register-tagged-literal! h)

(set-print-fn! #(.log js/console %))

(defn ff [] (h [:tbody {:e "e"} [:p [:tbody "e"]]]))

(comment
  (def cc (render! (new-root) ff))
  (node-to-string (root))
  )

(def ^:dynamic *comp* nil)

(multi-defn simple1 [x] (h [:div#ii.cc {} x 4]))
(multi-defn simple2 [x y z] (h [x y z]))
(multi-defn simple3 [x] #h [:div#ii.cc x])
(multi-defn simple4 [] #h [:div "<content"])
(multi-defn simple5 [x] #h [x "content"])

#_(deftest test-simple1
  (testing "test-simple1"
    (let [comp-fn simple-comp1
          comp-fn-string simple-comp1-string
          comp (render! (new-root) comp-fn "e")]
      (is (= (.-innerHTML (root))
             (str (comp-fn-string "e"))))
      (update! comp comp-fn "f")
      (is (= (.-innerHTML (root))
             (str (comp-fn-string "f"))))
      (update! comp comp-fn "g")
      (is (= (.-innerHTML (root))
             (str (comp-fn-string "g")))))))

#_(deftest test-simple2
  (testing "test-simple2"
    (let [comp-fn simple-comp2
          comp-fn-string simple-comp2-string
          comp (render! (new-root) comp-fn :p {:e "e"} "t")]
      (is (= (.-innerHTML (root))
             (str (comp-fn-string :p {:e "e"} "t"))))
      (update! comp comp-fn :p {:class "c2"} "t")
      (is (= (.-innerHTML (root))
             (str (comp-fn-string :p {:class "c2"} "t"))))
      (update! comp comp-fn :p {:class "c2" :e "e"} "t")
      (is (= (.-innerHTML (root))
             (str (comp-fn-string :p {:class "c2" :e "e"} "t")))))))

#_(deftest test-simple3
  (testing "test-simple3"
    (let [comp-fn simple-comp3
          comp-fn-string simple-comp3-string
          comp (render! (new-root) comp-fn "e")]
      (is (= (.-innerHTML (root))
             (str (comp-fn-string "e"))))
      (update! comp comp-fn "f")
      (is (= (.-innerHTML (root))
             (str (comp-fn-string "f"))))
      (update! comp comp-fn {:id "i"})
      (is (= (.-innerHTML (root))
             (str (comp-fn-string {:id "i"})))))))

(comment
  (let [comp-fn simple-comp4
        comp-fn-string simple-comp4-string
        comp (render! (new-root) comp-fn)]
    (.-innerHTML (root))
    #_(str (comp-fn-string))
    #_(update! comp comp-fn)
    #_(str (comp-fn-string)))
  )

(comment
  (node-to-string (root))
  )

#_(deftest test-simple5
  (testing "test-simple5"
    (let [comp-fn simple-comp5
          comp-fn-string simple-comp5-string
          comp (render! (new-root) comp-fn :p)]
      (is (= (.-innerHTML (root))
             (str (comp-fn-string :p))))
      (update! comp comp-fn :div)
      (is (= (.-innerHTML (root))
             (str (comp-fn-string :div))))
      (update! comp comp-fn :p)
      (is (= (.-innerHTML (root))
             (str (comp-fn-string :p))))
      (update! comp comp-fn :input)
      #_(is (= (.-innerHTML (root))
               (str (comp-fn-string :input))))
      (.-innerHTML (root))
      (str (comp-fn-string :input)))))

(comment

  (def cc (render! (new-root) def5 :p))
  (update! cc def5 :div)
  (update! cc def5 :p)
  (update! cc def5 :input)
  )

(defn template1 [x] #h [:p#ii.cc {:e x :class x} x "4"])
(defn template2 [x z] #h [:p {} (count x) #h [:p z]
                          (for [y x] (template1 y))])

(comment
  (def cc (render! (new-root) template2 (list 1 2) nil))
  (update! cc template2 (list 1 3) #h [:div])
  (update! cc template2 (list 4) {:class "c"})
  (update! cc template2 (list 4) {:class "e"})
  )

(defn template3 [x] #h [:p {:class x} nil x])
(defn template4 [x] #h [:p {}
                        (for [y x]
                          (with-opts! {:key y}
                            (template3 (inc y))))])
(defn template44 [x z] (let [cc #h [:p]]
                         #h [:div [:p {}
                                   (list
                                    (for [y x]
                                      (if (and (= 2 y) z)
                                        (with-opts! {:key "tt"} cc)
                                        (with-opts! {:key y}
                                          (template3 (inc y)))))
                                    5)]
                             (when (not z)
                               (with-opts! {:key "tt"} cc))]))

(comment
  (def cc (render! (new-root) template4 (list 1 2)))
  (update! cc template4 (list 2 1))
  (update! cc template4 (list 1 2))
  #_(def cc (render! (new-root) template4 (list 1 2)))
  (def cc (render! (new-root) template4 (list 2 3 0)))
  (update! cc template4 (list 0 1))
  #_(update! cc template4 (list 3 0 1))
  (update! cc template4 (list 2 3 0))
  (update! cc template4 (list 3 0 1))
  (def ll (atom (cycle (range 20))))
  (let [n (take 19 @ll)]
    (update! cc template4 n)
    (do (swap! ll #(drop 19 %)) nil))

  (def cc (render! (new-root) template44 (list 1 2 3) true))
  (update! cc template44 (list 1 3 2) true)
  (update! cc template44 (list 1 3 2) false)
  (update! cc template44 (list 1 2 3) true)
  )

(defn template5 [x y] #h [:p {}
                          (let [cc #h [:div "else"]]
                            (if y
                              #h [:div (with-opts! {:key 1} cc)]
                              (with-opts! {:key 1} cc)))])

(comment
  (def cc (render! (new-root) template5 3 false))
  (update! cc template5 3 false)
  (update! cc template5 3 true)
  )

#_(deftest keyedChildren
  (testing "keyed children"
    (let [comp (template4 (list 1 2))]
      (create-comp comp)
      (.log js/console @(update-comp (template4 (list 2 1)) comp)))))


(comment
  (run-tests 'ewen.inccup.incremental.core-test)

  )



(defn large [a b] #h [:div {} "e" [:p {} "e" 3] [:p {} "e" 3 [:p {} "e" 3 [:p {} "e" 3]]] [:p {} "e" 3 [:p {} "e" 3 [:p {} "e" 3 [:p {} "e" 3 [:p {} "e" 3 [:p {} "e" 3 [:p {} "e" 3 [:p {} "e" 3] [:p {} "e" 3] [:p {} "e" 3] [:p {} "e" 3] [:p {} "e" 3] [:p {} "e" 3] [:p {} "e" 3] [:p {} "e" 3] [:p {} "e" 3 [:p {} "e" 3 [:p {} "e" 3 [:p {} a 3 [:p {} "e" 3 [:p {} "e" 3 [:p {} "e" 3 [:p {} "e" 3 [:p {} "e" 3 [:p {} "e" 3 [:p {} "e" b]]]]]]]]]]]]]]]]]] [:p {} "e" 3]])



(comment
  (def cc (render! (new-root) large #h [:div 1 2 3] 3))
  (update! cc large (large (large 3 (large 5 6)) 2) (large 3 4))

  )


(comment
  (def cc (render! (new-root) (fn [] (h [:div {} (clojure.core/list nil)]))))
  (node-to-string (root))
  )
