(ns ewen.inccup.incremental.compiler-macros
  (:require [ewen.inccup.incremental.emitter :as emitter]
            [ewen.inccup.util :as util]
            [cljs.analyzer.api :as ana-api]
            [clojure.walk :refer [postwalk]]
            [clojure.set :refer [union]]
            [cljs.tagged-literals :refer [*cljs-data-readers*]]
            [clojure.pprint :refer [pprint pp]]))

(def ^:dynamic *dynamic-forms* nil)
(def ^:dynamic *tracked-vars* #{})
(def ^:dynamic *env* nil)
(def ^:dynamic *update-paths* [])
(def ^:dynamic *skips* [])
(def ^:dynamic *statics* [])
(defonce component-id (atom 0))

(deftype DynamicLeaf [index])

(defn track-vars [expr]
  (binding [*tracked-vars* *tracked-vars*]
    (let [env (or *env* (ana-api/empty-env))
          cljs-expanded (-> env (ana-api/analyze expr) emitter/emit*)
          used-vars (keep #(when (:is-used %) (:symbol %))
                          (vals *tracked-vars*))]
      [cljs-expanded (set used-vars)])))

(defn update-dynamic-forms [expr path used-vars]
  (when *dynamic-forms*
    (set! *dynamic-forms*
          (conj *dynamic-forms*
                {:path path
                 :var-deps used-vars
                 :form expr
                 :indexes #{(count *dynamic-forms*)}}))))

(defn compile-dynamic-expr [expr path]
  (let [[expr used-vars] (track-vars expr)]
    (update-dynamic-forms expr path used-vars)
    (->DynamicLeaf (-> *dynamic-forms* count dec))))

(defn compile-attr-map [attrs path]
  (compile-dynamic-expr attrs path)
  nil)

(defn maybe-attr-map
  ([attrs attr-path path tag-attrs]
   {:pred [(not (some util/unevaluated? (mapcat identity attrs)))]}
   (when attrs
     (let [[expr used-vars] (track-vars attrs)
           attr-form
           `(ewen.inccup.incremental.compiler/maybe-merge-attributes
             ~tag-attrs ~expr)
           form `(if (map? ewen.inccup.incremental.compiler/*tmp-val*)
                   nil
                   ewen.inccup.incremental.compiler/*tmp-val*)]
       (update-dynamic-forms attr-form attr-path used-vars)
       (update-dynamic-forms form path used-vars)
       nil))))

(defn- literal?
  "True if x is a literal value that can be rendered as-is."
  [x]
  (and (not (util/unevaluated? x))
       (or (not (or (vector? x) (map? x)))
           (every? literal? x))))

(defn dynamic-tag [tag path]
  {:pred [(not (literal? tag))]}
  (let [[expr used-vars] (track-vars tag)]
    (update-dynamic-forms expr path used-vars)
    nil))

(defn- form-name
  "Get the name of the supplied form."
  [form]
  (if (and (seq? form) (symbol? (first form)))
    (name (first form))))

(declare compile-html)

(defn- not-hint?
  "True if x is not hinted to be the supplied type."
  [x type]
  (if-let [hint (-> x meta :tag)]
    (not (isa? (eval hint) type))))

(defn- hint?
  "True if x is hinted to be the supplied type."
  [x type]
  (if-let [hint (-> x meta :tag)]
    (isa? (eval hint) type)))

(defn- not-implicit-map?
  "True if we can infer that x is not a map."
  [x]
  (or (= (form-name x) "for")
      (not (util/unevaluated? x))
      (not-hint? x java.util.Map)))

(defn coll->js-array [coll]
  (if (coll? coll)
    `(cljs.core/array ~@(map coll->js-array coll))
    coll))

(defn literal->js [x]
  (cond
    (nil? x) nil
    (string? x) x
    (number? x) (str x)
    (keyword? x) (name x)
    (symbol? x) (str x)
    (vector? x) (if (instance? DynamicLeaf (first x))
                  (let [[tag attrs & content] x]
                    `(cljs.core/array ~(.-index tag)
                                      ~(literal->js attrs)
                                      ~@(map literal->js content)))
                  (let [[tag attrs content] (util/normalize-element x)]
                    `(cljs.core/array ~tag
                                      ~(literal->js attrs)
                                      ~@(map literal->js content))))
    (map? x) `(cljs.core/js-obj
               ~@(interleave (map name (keys x))
                             (map literal->js (vals x))))
    (instance? DynamicLeaf x) (.-index x)
    :else (throw (IllegalArgumentException.
                  (str "Not a literal element: " x)))))

(comment
  (literal->js '[:e.c {:class "c2"} "e"])
  )

(defn- element-compile-strategy
  "Returns the compilation strategy to use for a given element."
  [[tag attrs & content :as element] path]
  (cond
    (every? literal? element)
    ::all-literal
    (and (literal? tag)
         (map? attrs)
         (every? literal? attrs))
    ::literal-tag-and-literal-attributes
    (and (literal? tag)
         (map? attrs))
    ::literal-tag-and-map-attributes
    (and (literal? tag) (not-implicit-map? attrs))
    ::literal-tag-and-no-attributes
    (literal? tag)
    ::literal-tag
    (and (map? attrs) (every? literal? attrs))
    ::literal-attributes
    (map? attrs)
    ::map-attributes
    (not-implicit-map? attrs)
    ::no-attributes
    :else
    ::default))

(declare compile-seq)

(defmulti compile-element
  {:private true}
  element-compile-strategy)

(defmethod compile-element ::all-literal
  [element path] element)

(defmethod compile-element ::literal-tag-and-literal-attributes
  [[tag attrs & content] path]
  (let [[tag attrs _] (util/normalize-element [tag attrs])]
    (into [tag attrs] (compile-seq content path 2))))

(defmethod compile-element ::literal-tag-and-map-attributes
  [[tag attrs & content] path]
  (let [[tag attrs _] (util/normalize-element [tag attrs])]
    (compile-attr-map attrs (conj path 1))
    (into [tag (-> *dynamic-forms* count dec ->DynamicLeaf)]
          (compile-seq content path 2))))

(defmethod compile-element ::literal-tag-and-no-attributes
  [[tag & content] path]
  (compile-element (apply vector tag {} content) path))

(defmethod compile-element ::literal-tag
  [[tag attrs & content :as element] path]
  (let [[tag tag-attrs [first-content & rest-content]]
        (util/normalize-element element)]
    (maybe-attr-map first-content (conj path 1) (conj path 2) tag-attrs)
    (into [tag (-> *dynamic-forms* count dec dec ->DynamicLeaf)
           (-> *dynamic-forms* count dec ->DynamicLeaf)]
          (compile-seq rest-content path 3))))

(defmethod compile-element ::literal-attributes
  [[tag attrs & rest-content] path]
  (dynamic-tag tag (conj path 0))
  (into [(-> *dynamic-forms* count dec ->DynamicLeaf) attrs]
        (compile-seq rest-content path 2)))

(defmethod compile-element ::map-attributes
  [[tag attrs & rest-content] path]
  (dynamic-tag tag (conj path 0))
  (compile-attr-map attrs (conj path 1))
  (into [(-> *dynamic-forms* count dec dec ->DynamicLeaf)
         (-> *dynamic-forms* count dec ->DynamicLeaf)]
        (compile-seq rest-content path 2)))

(defmethod compile-element ::no-attributes
  [[tag & content] path]
  (compile-element (apply vector tag {} content) path))

(defmethod compile-element ::default
  [[tag attrs & rest-content :as element] path]
  (dynamic-tag tag (conj path 0))
  (let [tag-index (-> *dynamic-forms* count dec ->DynamicLeaf)]
    (if (= 1 (count element))
      [tag-index {}]
      (do
        (maybe-attr-map attrs (conj path 1) (conj path 2) {})
        (into [tag-index
               (-> *dynamic-forms* count dec dec ->DynamicLeaf)
               (-> *dynamic-forms* count dec ->DynamicLeaf)]
              (compile-seq rest-content path 3))))))

(declare compile-dispatch)

(defn- compile-seq
  "Compile a sequence of data-structures into HTML."
  ([content path]
   (compile-seq content path 0))
  ([content path index-init]
   (loop [[expr & rest-content :as content] content
          index index-init
          compiled []]
     (if (not (empty? content))
       (let [compiled-expr (compile-dispatch expr (conj path index))]
         (recur rest-content (inc index) (conj compiled compiled-expr)))
       compiled))))

(defn- compile-dispatch [expr path]
  (cond
    (vector? expr) (compile-element expr path)
    (string? expr) expr
    (keyword? expr) expr
    (literal? expr) expr
    (seq? expr) (compile-dynamic-expr expr path)
    :else (compile-dynamic-expr expr path)))

(defn compute-common-path [dynamic-forms]
  (let [indexes (apply union (map :indexes dynamic-forms))
        path-groups (->> (map :path dynamic-forms)
                         (apply interleave)
                         (partition (count dynamic-forms)))
        common-path (loop [[path-group & rest-path-groups] path-groups
                           common-path []]
                      (if (or (nil? path-group) (not (apply = path-group)))
                        common-path
                        (recur rest-path-groups
                               (conj common-path (first path-group)))))]
    [common-path indexes]))

(defn remove-from-path [n form]
  {:post [(-> (:path %) empty? not)]}
  (update form :path subvec n))

(defn group-dynamic-forms [dynamic-forms]
  (let [partitioned (partition-by (comp first :path) dynamic-forms)
        forms (for [forms partitioned]
                (if (= 1 (count forms))
                  (first forms)
                  (let [[common-path indexes] (compute-common-path forms)
                        n (count common-path)
                        updated-forms (map
                                       (partial remove-from-path n)
                                       forms)]
                    {:path common-path :indexes indexes
                     :sub-forms (group-dynamic-forms updated-forms)})))]
    forms))

(comment
  (compute-common-path [{:path [2 0 0]
                         :var-deps #{1}
                         :indexes #{0}}
                        {:path [2 0 1 0 0]
                         :var-deps #{1 2}
                         :indexes #{1}}
                        {:path [2 0 1 0 1]
                         :var-deps #{1 2}
                         :indexes #{2}}])

  (group-dynamic-forms '[{:path [2 0 0]
                          :var-deps #{x}
                          :form x
                          :indexes #{0}}
                         {:path [2 0 1 0 0]
                          :var-deps #{x y}
                          :form #(update-in % y)
                          :indexes #{1}}
                         {:path [2 0 1 0 1]
                          :var-deps #{z}
                          :form z
                          :indexes #{2}}])

  (group-dynamic-forms '[{:path [0 1]
                          :var-deps #{x}
                          :form x
                          :indexes #{0}}
                         {:path [2 0 0]
                          :var-deps #{x}
                          :form x
                          :indexes #{1}}
                         {:path [2 0 1 0 0]
                          :var-deps #{x y}
                          :form #(update-in % y)
                          :indexes #{2}}
                         {:path [2 0 1 0 1]
                          :var-deps #{z}
                          :form z
                          :indexes #{3}}])

  ;; Should throw an error
  (group-dynamic-forms '[{:path [0 1] :var-deps #{x} :form x}
                         {:path [2 0 1] :var-deps #{x} :form x}
                         {:path [2 0 1 0 0]
                          :var-deps #{x y}
                          :form y}
                         {:path [2 0 1 0 1]
                          :var-deps #{z}
                          :form z}])
  )

(defn dynamic-form->update-path [{:keys [path indexes sub-forms]}]
  (let [indexes (-> indexes coll->js-array)
        sub-paths (map dynamic-form->update-path sub-forms)]
    `(ewen.inccup.incremental.compiler/array-with-path
      ~indexes (cljs.core/array ~@path ~@sub-paths))))

(comment
  (dynamic-form->update-path
   '{:path [2 0],
     :indexes #{1 3 2},
     :sub-forms
     ({:path [0], :var-deps #{x}, :form x, :indexes #{1}}
      {:path [1 0],
       :indexes #{3 2},
       :sub-forms
       ({:path [0],
         :var-deps #{x y},
         :form (fn* [p1__14475#] (update-in p1__14475# y)),
         :indexes #{2}}
        {:path [1], :var-deps #{z}, :form z, :indexes #{3}})})})
  )

(defn dynamic-forms->update-path [forms]
  (cond
    (= 0 (count forms)) nil
    (= 1 (count forms)) (dynamic-form->update-path (first forms))
    :else
    (let [indexes (->> (map :indexes forms)
                       (apply union)
                       coll->js-array)
          update-paths (map dynamic-form->update-path forms)]
      `(ewen.inccup.incremental.compiler/array-with-path
        ~indexes
        (cljs.core/array ~@update-paths)))))

(comment
  (dynamic-forms->update-path
   '({:path [0 1], :var-deps #{x}, :form x, :indexes #{0}}
     {:path [2 0],
      :indexes #{1 3 2},
      :sub-forms
      ({:path [0], :var-deps #{x}, :form x, :indexes #{1}}
       {:path [1 0],
        :indexes #{3 2},
        :sub-forms
        ({:path [0],
          :var-deps #{x y},
          :form (fn* [p1__15207#] (update-in p1__15207# y)),
          :indexes #{2}}
         {:path [1], :var-deps #{z}, :form z, :indexes #{3}})})}))
  )

(defn var-deps->indexes [params var-deps]
  (map #(.indexOf params %) var-deps))

(defn form-with-var-dep [var-deps-sym index form]
  `(when (cljs.core/aget ~var-deps-sym ~index) ~form))

(defmacro component [forms]
  (let [params (->> (:locals &env)
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
        (binding [*env* &env
                  *dynamic-forms* []
                  *tracked-vars* tracked-vars]
          [(compile-dispatch forms []) *dynamic-forms*])
        static (literal->js static)
        update-path (-> (group-dynamic-forms dynamic)
                        dynamic-forms->update-path)
        var-deps->indexes (partial var-deps->indexes (keys tracked-vars))
        id (swap! component-id inc)]
    `(ewen.inccup.incremental.compiler/->Component
      ~id 1
      (ewen.inccup.incremental.compiler/get-or-set-global
       ~id "static" ~static)
      (ewen.inccup.incremental.compiler/get-or-set-global
       ~id "update-path" ~update-path)
      ~(coll->js-array (keys tracked-vars))
      (ewen.inccup.incremental.compiler/get-or-set-global
       ~id "var-deps" ~(->> dynamic (map :var-deps)
                            (map var-deps->indexes)
                            coll->js-array))
      nil 1
      ~(let [var-deps-sym (gensym "var-deps")]
         `(fn [~var-deps-sym]
            (cljs.core/array
             ~@(->> dynamic
                    (map :form)
                    (map-indexed
                     (partial form-with-var-dep var-deps-sym)))))))))

(alter-var-root #'*cljs-data-readers* assoc 'h
                (fn [forms]
                  {:pre [(vector? forms)]}
                  `(component ~forms)))

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
