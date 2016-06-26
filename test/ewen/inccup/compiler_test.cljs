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
    (update! comp "f")
    #_(.-innerHTML (root))
    #_(str (simple1 "f")))
  )

(comment
  (let [comp-fn simple4
        comp-fn-string simple4-string
        comp (render! (new-root) comp-fn)]
    (.-innerHTML (root))
    #_(str (comp-fn-string))
    #_(update! comp)
    #_(str (comp-fn-string)))
  )

(comment
  (node-to-string (root))
  )

(comment

  (def cc (render! (new-root) simple5 :p))
  (update! cc :div)
  (update! cc :p)
  (update! cc :input)
  )

(multi-defn list1* [x] (h [:p#ii.cc {:e x :class x} x "4"]))
(multi-defn list1 [x z] (h [:p {} (count x) (h [:p z])
                            (for [y x] (list1* y))]))

(comment
  (def cc (render! (new-root) list1 (list 1 2) nil))
  (update! cc (list 1 3) (h [:div]))
  (update! cc (list 4) {:class "c"})
  (update! cc (list 4) {:class "e"})
  )

(defn keyed1* [x] (h [:p {:class x} nil x]))
(defn keyed1 [x] (h [:p {}
                     (for [y x]
                       (with-opts! {:key y}
                         (keyed1* (inc y))))]))
#_(defn keyed2 [x z] (let [cc (h [:p])]
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
  (def cc (render! (new-root) keyed1 (list 1 2)))
  (update! cc (list 2 1))
  (update! cc (list 1 2))
  (def cc (render! (new-root) keyed1 (list 2 3 0)))
  (update! cc (list 0 1))
  #_(update! cc (list 3 0 1))
  (update! cc (list 2 3 0))
  (update! cc (list 3 0 1))
  (def ll (atom (cycle (range 20))))
  (let [n (take 19 @ll)]
    (update! cc n)
    (do (swap! ll #(drop 19 %)) nil))

  #_(def cc (render! (new-root) template44 (list 1 2 3) true))
  #_(update! cc (list 1 3 2) true)
  #_(update! cc (list 1 3 2) false)
  #_(update! cc (list 1 2 3) true)
  )

(defn keyed3 [x y] (h [:p {}
                       (let [cc (h [:div "else"])]
                         (if y
                           (h [:div (with-opts! {:key 1} cc)])
                           (with-opts! {:key 1} cc)))]))

(comment
  (def cc (render! (new-root) keyed3 3 false))
  (update! cc 3 false)
  (update! cc 3 true)
  )


(comment
  (def cc (render! (new-root)
                   (fn [] (h [:div {} [:iframe "e"]]))))
  (node-to-string (root))
  )
