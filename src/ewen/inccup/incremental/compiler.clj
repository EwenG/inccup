(ns ewen.inccup.incremental.compiler
  (:require [ewen.inccup.incremental.emitter :as emitter]
            [ewen.inccup.common.util :as util]
            [ewen.inccup.common.compiler :as c-comp]
            [cljs.analyzer.api :as ana-api]
            [clojure.walk :refer [postwalk]]
            [clojure.set :refer [union]]
            [clojure.pprint :refer [pprint pp]]
            [clojure.tools.reader :as reader])
  (:import [ewen.inccup.common.compiler DynamicLeaf]))

(def ^:dynamic *component-locals* nil)
(def ^:dynamic *dynamic-expr-vars* nil)
(defonce component-id (atom 0))

(defn collect-input-var-deps
  ([collected-vars analyzed-var]
   (collect-input-var-deps nil collected-vars analyzed-var))
  ([component-locals collected-vars
    {name :name fn-var :fn-var
     local :local init :init
     op :op tag :tag
     :as info}]
   (cond
     (and (= :var op)
          (not local)
          (not fn-var)
          (or (nil? component-locals)
              (contains? component-locals name))
          (or (nil? component-locals)
              (identical? info (get-in component-locals [name :info]))))
     (assoc collected-vars name {:info info})
     ;; The analyzed var is a local var with an initial binding. The
     ;; initial binding may itself depend on a function parameter,
     ;; thus we recurse on the initial binding
     (and (= :var op) local init)
     (if (not (empty? (:children init)))
       ;; If init has children, then it is a complex expression, by
       ;; opposition to a single var or local binding. Recurse on every
       ;; child in order to analyze each expression individually
       (->> (:children init)
            (mapv :info)
            (mapv (partial collect-input-var-deps
                           component-locals collected-vars))
            (apply merge))
       (collect-input-var-deps component-locals collected-vars
                               (:info init)))
     :else collected-vars)))

(defn track-vars [env component-locals expr]
  (binding [*component-locals* component-locals
            *dynamic-expr-vars* #{}]
    (let [cljs-expanded (-> env (ana-api/analyze expr) emitter/emit*)
          used-vars *dynamic-expr-vars*]
      [cljs-expanded (set used-vars)])))

(declare compile-html)

(defn literal->js [x]
  (cond
    (keyword? x) (literal->js (name x))
    (vector? x)
    (let [[tag attrs & content] x]
      `(cljs.core/array ~(literal->js tag)
                        ~(literal->js attrs)
                        ~@content))
    (map? x) `(cljs.core/js-obj
               ~@(interleave (map name (keys x))
                             (vals x)))
    (instance? DynamicLeaf x) (.-index x)
    :else (str x)))

(defn dynamic-form-with-tracked-vars
  [env component-locals {:keys [original-form] :as dynamic-form}]
  (let [[expr used-vars] (track-vars env component-locals original-form)]
    (assoc dynamic-form :var-deps used-vars)))

(defn dynamic-forms-with-tracked-vars [env component-locals dynamic]
  (into []
        (map (partial dynamic-form-with-tracked-vars env component-locals))
        dynamic))

(defn indexed-identity [index form]
  [index form])

(defn var-deps->indexes [tracked-vars var-deps]
  (map #(.indexOf tracked-vars %) var-deps))

(defn component [env forms]
  (let [component-locals (loop [component-locals {}
                                locals (:locals env)]
                           (if-let [local (first locals)]
                             (recur (->> (second local)
                                         (collect-input-var-deps {})
                                         (merge component-locals))
                                    (rest locals))
                             component-locals))
        [static dynamic]
        (binding [c-comp/*dynamic-forms* []]
          [(c-comp/compile-dispatch forms [] true)
           c-comp/*dynamic-forms*])
        dynamic (dynamic-forms-with-tracked-vars
                 env component-locals dynamic)
        tracked-vars (->> (map :var-deps dynamic)
                          (apply union)
                          (into []))
        static (c-comp/walk-static identity literal->js static)
        var-deps->indexes (partial var-deps->indexes tracked-vars)
        id (swap! component-id inc)]
    `(ewen.inccup.incremental.vdom/->Component
      ~id
      (cljs.core/or
       (goog.object/get
        (goog.object/get
         ewen.inccup.incremental.vdom/*globals* ~id nil) "static")
       ~static)
      ~(c-comp/coll->js-array (map :name tracked-vars))
      (cljs.core/or
       (goog.object/get
        (goog.object/get
         ewen.inccup.incremental.vdom/*globals* ~id nil) "var-deps")
       ~(->> dynamic (map :var-deps)
             (map var-deps->indexes)
             c-comp/coll->js-array))
      (fn [var-deps-index#]
        (cljs.core/case var-deps-index#
          ~@(->> dynamic
                 (map :form)
                 (map-indexed indexed-identity)
                 (apply concat))))
      ~(count dynamic))))

(defmethod emitter/emit* :var
  [{:keys [info env form] :as ast}]
  (let [used-vars (collect-input-var-deps
                   *component-locals* {} info)]
    (doseq [[name {:keys [atom?]}] used-vars]
      (set! *dynamic-expr-vars*
            (conj *dynamic-expr-vars* {:name name}))))
  (emitter/var-emit ast))


(comment
  (binding [*component-locals* {'e false}]
    (emit* (ana-api/analyze (ana-api/empty-env)
                            '(let [e "e"]
                               e)))
    *component-locals*)
  )
