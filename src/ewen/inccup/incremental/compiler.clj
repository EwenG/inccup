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

(def ^:dynamic *tracked-vars* nil)
(defonce component-id (atom 0))

(defn track-vars [env tracked-vars expr]
  (binding [*tracked-vars* tracked-vars]
    (let [cljs-expanded (-> env (ana-api/analyze expr) emitter/emit*)
          used-vars (keep #(when (:is-used %) (:symbol %))
                          (vals tracked-vars))]
      [cljs-expanded (set used-vars)])))

(declare compile-html)

(defn literal->js [x]
  (cond
    (keyword? x) (literal->js (name x))
    (vector? x)
    (let [[tag attrs & content] x]
      `(cljs.core/array ~(if (instance? DynamicLeaf tag)
                           (.-index tag)
                           (clojure.string/upper-case (name tag)))
                        ~(literal->js attrs)
                        ~@content))
    (map? x) `(cljs.core/js-obj
               ~@(interleave (map name (keys x))
                             (vals x)))
    (instance? DynamicLeaf x) (.-index x)
    :else (str x)))

(defn dynamic-form-with-tracked-vars
  [env tracked-vars {:keys [form] :as dynamic-form}]
  (let [[expr used-vars] (track-vars env tracked-vars form)]
    (assoc dynamic-form :var-deps used-vars)))

(defn dynamic-forms-with-tracked-vars [env tracked-vars dynamic]
  (into [] (map (partial dynamic-form-with-tracked-vars env tracked-vars))
        dynamic))

(defn static-with-metas [static {:keys [path index]}]
  (loop [static static
         rest-path path
         processed-path []]
    (if-let [path-index (first rest-path)]
      (if (empty? processed-path)
        (recur (vary-meta static update-in
                          [:update-paths] union #{index})
               (rest rest-path)
               (conj processed-path path-index))
        (recur (update-in static processed-path vary-meta update-in
                          [:update-paths] union #{index})
               (rest rest-path)
               (conj processed-path path-index)))
      static)))

(defn var-deps->indexes [params var-deps]
  (map #(.indexOf params %) var-deps))

(defn form-with-var-dep [var-deps-sym index form]
  `(when (cljs.core/aget ~var-deps-sym ~index) ~form))

(defn component [env forms]
  (let [params (->> (:locals env)
                    (filter (comp not :local second))
                    (filter (comp not :fn-var second)))
        tracked-vars (loop [tracked-vars {}
                            params params]
                       (if-let [[param-name param-env] (first params)]
                         (recur (assoc tracked-vars param-name
                                       {:env param-env
                                        :is-used false
                                        :symbol param-name})
                                (rest params))
                         tracked-vars))
        [static dynamic]
        (binding [c-comp/*dynamic-forms* []]
          [(c-comp/compile-dispatch forms []) c-comp/*dynamic-forms*])
        #_dynamic #_(dynamic-forms-with-tracked-vars
                 env tracked-vars dynamic)
        #_static #_(loop [static static
                      dynamic dynamic]
                 (if-let [update-path (first dynamic)]
                   (recur (static-with-metas static update-path)
                          (rest dynamic))
                   static))
        static (c-comp/walk-static
                c-comp/handle-void-tags literal->js static)
        #_var-deps->indexes #_(partial var-deps->indexes
                                       (keys tracked-vars))
        id (swap! component-id inc)]
    `(ewen.inccup.incremental.vdom2/->Component
      ~id
      (cljs.core/or
       (goog.object/get
        (goog.object/get
         ewen.inccup.incremental.vdom/*globals* ~id nil) "static")
       (ewen.inccup.incremental.vdom/tree-with-parents
        (cljs.core/array ~static)))
      ~(c-comp/coll->js-array (keys tracked-vars))
      (fn [] (cljs.core/array ~@(map :form dynamic))))))

(defn collect-input-var-deps
  [tracked-vars collected-vars
   {:keys [op local init info env children] :as ast}]
  (let [name (:name info)]
    (cond
      (and (= :var op)
           (contains? tracked-vars name)
           (identical? (get (:locals env) name)
                       (get-in tracked-vars [name :env])))
      (conj collected-vars ast)
      (and (= :var op) local init)
      (collect-input-var-deps tracked-vars collected-vars init)
      (not (empty? children))
      (->> (map (partial collect-input-var-deps
                         tracked-vars collected-vars)
                children)
           (apply union))
      :else collected-vars)))

(defmethod emitter/emit* :var
  [{:keys [info env form] :as ast}]
  (let [used-vars (collect-input-var-deps *tracked-vars* #{} ast)]
    (doseq [{{name :name} :info} used-vars]
      (set! *tracked-vars*
            (assoc-in *tracked-vars* [name :is-used] true))))
  (emitter/var-emit ast))


(comment
  (binding [*tracked-vars* {'e false}]
    (emit* (ana-api/analyze (ana-api/empty-env)
                            '(let [e "e"]
                               e)))
    *tracked-vars*)
  )
