(ns ewen.inccup.compiler-test
  (:require [cljs.test :refer-macros [deftest testing is run-tests]]
            [ewen.inccup.compiler :refer-macros [with-opts! h] :as comp]
            [ewen.inccup.incremental.vdom :as vdom
             :refer [Component render! update!]]
            [ewen.inccup.utils-test :refer [node-to-string root new-root]]
            [goog.dom])
  (:require-macros [ewen.inccup.compiler-test :refer [multi-defn]]))

(set-print-fn! #(.log js/console %))

(def ^:dynamic *cljs-output-mode* :incremental)
(def ^:dynamic *comp* nil)

(multi-defn simple1 [x] (h [:div#ii.cc {} x 4]))
(multi-defn simple2 [x y z] (h [x y z]))
(multi-defn simple3 [x] (h [:div#ii.cc x]))
(multi-defn simple4 [] (h [:div "<content"]))
(multi-defn simple5 [x] (h [x "content"]))

(comment
  (let [comp (render! (new-root) simple1 "e")]
    (.-innerHTML (root))
    (binding [*cljs-output-mode* :string]
      (str (simple1 "e")))
    #_(update! comp simple1 "f")
    #_(.-innerHTML (root))
    #_(str (simple1 "f")))
  )

(comment
  (let [comp-fn simple4
        comp-fn-string simple4-string
        comp (render! (new-root) comp-fn)]
    (.-innerHTML (root))
    #_(str (comp-fn-string))
    #_(update! comp comp-fn)
    #_(str (comp-fn-string)))
  )

(comment
  (node-to-string (root))
  )

(comment

  (def cc (render! (new-root) def5 :p))
  (update! cc def5 :div)
  (update! cc def5 :p)
  (update! cc def5 :input)
  )

(multi-defn list1* [x] (h [:p#ii.cc {:e x :class x} x "4"]))
(multi-defn list1 [x z] (h [:p {} (count x) (h [:p z])
                            (for [y x] (list1* y))]))

(comment
  (def cc (render! (new-root) list1 (list 1 2) nil))
  (update! cc list1 (list 1 3) (h [:div]))
  (update! cc list1 (list 4) {:class "c"})
  (update! cc list1 (list 4) {:class "e"})

  (binding [*cljs-output-mode* :string]
    (str (list1-string (list 1 2) nil)))

  (binding [*cljs-output-mode* :string]
    (str (list1-string (list 1 3) (h [:div]))))
  )

(defn template3 [x] (h [:p {:class x} nil x]))
(defn template4 [x] (h [:p {}
                        (for [y x]
                          (with-opts! {:key y}
                            (template3 (inc y))))]))
(defn template44 [x z] (let [cc (h [:p])]
                         (h [:div [:p {}
                                   (list
                                    (for [y x]
                                      (if (and (= 2 y) z)
                                        (with-opts! {:key "tt"} cc)
                                        (with-opts! {:key y}
                                          (template3 (inc y)))))
                                    5)]
                             (when (not z)
                               (with-opts! {:key "tt"} cc))])))

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

(defn template5 [x y] (h [:p {}
                          (let [cc (h [:div "else"])]
                            (if y
                              (h [:div (with-opts! {:key 1} cc)])
                              (with-opts! {:key 1} cc)))]))

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



(defn large [a b] (h [:div {} "e" [:p {} "e" 3] [:p {} "e" 3 [:p {} "e" 3 [:p {} "e" 3]]] [:p {} "e" 3 [:p {} "e" 3 [:p {} "e" 3 [:p {} "e" 3 [:p {} "e" 3 [:p {} "e" 3 [:p {} "e" 3 [:p {} "e" 3] [:p {} "e" 3] [:p {} "e" 3] [:p {} "e" 3] [:p {} "e" 3] [:p {} "e" 3] [:p {} "e" 3] [:p {} "e" 3] [:p {} "e" 3 [:p {} "e" 3 [:p {} "e" 3 [:p {} a 3 [:p {} "e" 3 [:p {} "e" 3 [:p {} "e" 3 [:p {} "e" 3 [:p {} "e" 3 [:p {} "e" 3 [:p {} "e" b]]]]]]]]]]]]]]]]]] [:p {} "e" 3]]))



(comment
  (def cc (render! (new-root) large (h [:div 1 2 3]) 3))
  (update! cc large (large (large 3 (large 5 6)) 2) (large 3 4))

  )


(comment
  (def cc (render! (new-root)
                   (fn [] (h [:div {} [:iframe "e"]]))))
  (node-to-string (root))
  )
