(ns ewen.inccup.incremental.core-test
  (:require [cljs.test :refer-macros [deftest testing is run-tests]]
            [ewen.inccup.core :refer-macros [with-opts]]
            [ewen.inccup.incremental.compiler :as comp
             :refer [Component render! update!]]
            [cljs.pprint :refer [pprint] :refer-macros [pp]]
            [goog.array]
            [goog.dom])
  (:require-macros [ewen.inccup.incremental.core-test-macros])
  (:refer-clojure :exclude [-equiv]))

(set-print-fn! #(.log js/console %))

;; node.isEqual is not ie8 compatible
(defn node= [node1 node2]
  (let [equal-nodes (volatile! true)]
    (if (not= (.-nodeName node1) (.-nodeName node2))
      (vreset! equal-nodes false)
      (let [attrs1 (.-attributes node1)
            attrs2 (.-attributes node2)]
        (when (not (and (nil? attrs1) (nil? attrs2)))
          (if (not= (.-length attrs1) (.-length attrs2))
            (vreset! equal-nodes false)
            (loop [index 0
                   l (.-length attrs1)]
              (when (and @equal-nodes (< index l))
                (when (not= (.-name (aget attrs1 index))
                            (.-name (aget attrs2 index)))
                  (vreset! equal-nodes false))
                (when (not= (.-value (aget attrs1 index))
                            (.-value (aget attrs2 index)))
                  (vreset! equal-nodes false))
                (recur (inc index) l)))))))
    (when @equal-nodes
      (let [children1 (.-childNodes node1)
            children2 (.-childNodes node2)]
        (if (not= (.-length children1) (.-length children2))
          (vreset! equal-nodes false)
          (loop [index 0
                 l (.-length children1)]
            (when (and @equal-nodes (< index l))
              (when-not (node= (aget children1 index)
                               (aget children2 index))
                (vreset! equal-nodes false))
              (recur (inc index) l))))))
    @equal-nodes))

(declare inccup=)

(defprotocol InccupEquiv
  (-equiv [o other]))

(deftype ComponentValue [value]
  InccupEquiv
  (-equiv [_ other]
    (if (instance? Component other)
      (inccup= value (.-value other))
      false)))

(extend-type Component
  IPrintWithWriter
  (-pr-writer [c writer opts]
    (-write writer "#inccup/ComponentValue ")
    (pr-writer (.-value c) writer opts)))

(extend-type Component
  InccupEquiv
  (-equiv [c other]
    (if (instance? ComponentValue other)
      (inccup= (.-value c) (.-value other))
      false)))

(extend-type array
  InccupEquiv
  (-equiv [a other]
    (if (instance? js/Array other)
      (goog.array/equals a other inccup=)
      false)))

(extend-type object
  InccupEquiv
  (-equiv [o other]
    (if (instance? js/Object other)
      (let [equal? (volatile! true)]
        (goog.object/forEach
         o (fn [v k _]
             (when-not (and (goog.object/containsKey other k)
                            (inccup= v (aget other k)))
               (vreset! equal? false))))
        (goog.object/forEach
         other (fn [v k _]
                 (when-not (goog.object/containsKey o k)
                   (vreset! equal? false))))
        @equal?)
      false)))

(extend-type default
  InccupEquiv
  (-equiv [x o] (= x o)))

(defn inccup=
  ([x] true)
  ([x y]
   (cond
     (nil? x)
     (nil? y)
     (implements? IEquiv x)
     (cljs.core/-equiv x y)
     :else
     (-equiv x y)))
  ([x y & more]
   (if (inccup= x y)
     (if (next more)
       (recur y (first more) (next more))
       (inccup= y (first more)))
     false)))

(defn new-root []
  (let [old-root (.getElementById js/document "root")
        new-root (goog.dom/createDom "div" #js {:id "root"})]
    (if old-root
      (goog.dom/replaceNode new-root old-root)
      (goog.dom/appendChild (.-body js/document) new-root))
    new-root))

(defn root []
  (.querySelector js/document "#root"))

(defn def1 [x] #h [:div#ii.cc {} x])
(defn def2 [x y z] #h [x y z])
(defn def3 [x] #h [:div#ii.cc x])

(comment
  (-> (render! (new-root) def1 "e")
      (update! def1 "f")
      (update! def1 "g"))

  (def cc (render! (new-root) def2 :p {:e "e"} "t"))
  (update! cc def2 :p {:class "c2"} "t")

  (-> (render! (new-root) def2 :p {:class "c"} "t")
      (update! def2 :p {:class "c2" :e "e"} "t"))

  (-> (render! (new-root) def3 "e")
      (update! def3 "f")
      (update! def3 {:id "i"}))
  )

#_(deftest test1
  (testing "test1"
    (let [comp (def1 "e")]
      (is (inccup= @(create-comp comp)
                   #js ["div" #js {:id "ii", :class "cc"} "e"]))
      (is (inccup= @(update-comp (def1 3) comp)
                   #js ["div" #js {:id "ii", :class "cc"} "3"])))
    (let [comp (def2 :p {:e "e"} "t")]
      (is (inccup= @(create-comp comp)
                   #js ["p" #js {:e "e"} nil "t"]))
      (update-comp (def2 'div {:f "f"} "t") comp)
      (is (inccup= @comp
                   #js ["div" #js {:f "f"} nil "t"]))
      (update-comp (def2 'div 5 "f") comp)
      (is (inccup= @comp
                   #js ["div" #js {} "5" "f"])))))

(defn template1 [x] #h[:p#ii.cc {:e x :class x} x "4"])
(defn template2 [x z] #h [:p {} (count x) #h [:p z]
                          (for [y x] (template1 y))])

(comment
  (def cc (render! (new-root) template2 (list 1 2) nil))
  (update! cc template2 (list 1 3) #h [:div])
  (update! cc template2 (list 4) {:class "c"})
  (update! cc template2 (list 4) {:class "e"})
  )

#_(deftest test2
  (testing "test2"
    (let [comp (template2 (list 1 2) nil)]
      (binding [*effects* []]
        (is (inccup=
             @(create-comp comp)
             #js ["p" #js {} "2"
                  #inccup/ComponentValue #js ["p" #js {} nil]
                  #js [#inccup/ComponentValue
                       #js ["p" #js {:id "ii", :class "cc 1", :e "1"}
                            "1" "4"]
                       #inccup/ComponentValue
                       #js ["p" #js {:id "ii", :class "cc 2", :e "2"}
                            "2" "4"]]]))
        #_(pprint (filter #(= (first %) :mount-comp) *effects*))
        #_(pprint *effects*))
      (update-comp (template2 (list 1 #h [:div] "e") {:id 3}) comp)
      (is (inccup=
           @comp
           #js ["p" #js {} "3"
                #inccup/ComponentValue
                #js ["p" #js {:id "3"} nil]
                #js [#inccup/ComponentValue
                     #js ["p" #js {:id "ii", :class "cc 1", :e "1"} "1" "4"]
                     #inccup/ComponentValue
                     #js ["p"
                          #js {:id "ii", :class "cc [object Object]", :e "[object Object]"}
                          #inccup/ComponentValue #js ["div" #js {}] "4"]
                     #inccup/ComponentValue
                     #js ["p"
                          #js {:id "ii", :class "cc e", :e "e"}
                          "e" "4"]]]))
      (update-comp (template2 (list nil) {:class "4" :f "f"}) comp)
      (is (inccup=
           @comp
           #js ["p" #js {} "1"
                #inccup/ComponentValue
                #js ["p" #js {:class "4", :f "f"} nil]
                #js [#inccup/ComponentValue
                     #js ["p" #js {:id "ii", :class "cc ", :e ""}
                          nil "4"]]])))))

(defn template3 [x] #h [:p {:class x} nil x])
(defn template4 [x] #h [:p {}
                        (for [y x]
                          (with-opts {:key y}
                            (template3 (inc y))))])

(comment
  (def cc (render! (new-root) template4 (list 1 2)))
  (update! cc template4 (list 2 1))
  )

#_(deftest keyedChildren
  (testing "keyed children"
    (let [comp (template4 (list 1 2))]
      (create-comp comp)
      (.log js/console @(update-comp (template4 (list 2 1)) comp)))))


(comment
  (run-tests 'ewen.inccup.incremental.core-test)

  )


(comment


  )
