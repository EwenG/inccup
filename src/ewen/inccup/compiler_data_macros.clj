(ns ewen.inccup.compiler-data-macros
  (:require [ewen.inccup.compiler-data :as comp]
            [ewen.inccup.emitter :refer [*tracked-vars* track-vars]]
            [cljs.analyzer.api :refer [analyze empty-env]]
            [clojure.walk :refer [postwalk]]
            [clojure.set :refer [union]]))

(def ^:dynamic *cache-counter* nil)
(def ^:dynamic *dynamic-forms* nil)

(defn dynamic-result [expr path]
  (when *dynamic-forms*
    (binding [*tracked-vars* #{}]
      (let [cljs-expanded (track-vars expr)]
        (if (empty? *tracked-vars*)
          cljs-expanded
          (set! *dynamic-forms*
                (conj *dynamic-forms*
                      {:path path
                       :var-deps *tracked-vars*
                       :form cljs-expanded})))))))

(defn- unevaluated?
  "True if the expression has not been evaluated."
  [expr]
  (or (symbol? expr)
      (and (seq? expr)
           (not= (first expr) `quote))))

(defn compile-attr-map
  "Returns an unevaluated form that will render the supplied map as HTML
  attributes."
  [attrs path]
  (if (some unevaluated? (mapcat identity attrs))
    (dynamic-result attrs path) attrs))

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

(defn- literal?
  "True if x is a literal value that can be rendered as-is."
  [x]
  (and (not (unevaluated? x))
       (or (not (or (vector? x) (map? x)))
           (every? literal? x))))

(defn- not-implicit-map?
  "True if we can infer that x is not a map."
  [x]
  (or (= (form-name x) "for")
      (not (unevaluated? x))
      (not-hint? x java.util.Map)))

(defn- element-compile-strategy
  "Returns the compilation strategy to use for a given element."
  [[tag attrs & content :as element] path]
  (cond
    (every? literal? element)
    ::all-literal                    ; e.g. [:span "foo"]
    (and (literal? tag) (map? attrs))
    ::literal-tag-and-attributes     ; e.g. [:span {} x]
    (and (literal? tag) (not-implicit-map? attrs))
    ::literal-tag-and-no-attributes  ; e.g. [:span ^String x]
    (literal? tag)
    ::literal-tag                    ; e.g. [:span x]
    :else
    ::default))

(declare compile-seq)

(defmulti compile-element
  {:private true}
  element-compile-strategy)

(defmethod compile-element ::all-literal
  [element path] element)

(defmethod compile-element ::literal-tag-and-attributes
  [[tag attrs & content] path]
  (let [compiled-attrs (compile-attr-map attrs (conj path 1))]
    (if (#'comp/container-tag? tag content)
      [tag compiled-attrs (compile-seq content (conj path 2))]
      [tag compiled-attrs])))

(defn- compile-seq
  "Compile a sequence of data-structures into HTML."
  [content path]
  (loop [[expr & rest-content] content
         index 0
         compiled []]
    (if expr
      (let [compiled-expr
            (cond
              (vector? expr) (compile-element expr (conj path index))
              :else (dynamic-result expr (conj path index)))]
        (recur rest-content (inc index) (conj compiled compiled-expr)))
      ;; Retun a vector instead of a seq because it must be
      ;; compatible with update-in
      compiled)))

#_(defn group-nested-paths [grouped-path
                          {:keys [path tx nested-tx] :as dyn-element}]
  (let [updated-path (conj grouped-path path)]
    (if (or tx (not (= 1 (count nested-tx))))
      [updated-path dyn-element]
      (recur updated-path (first nested-tx)))))

(defn compute-common-path [dynamic-forms]
  (let [var-deps (apply union (map :var-deps dynamic-forms))
        path-groups (->> (map :path dynamic-forms)
                         (apply interleave)
                         (partition (count dynamic-forms)))]
    [(loop [[path-group & rest-path-groups] path-groups
            common-path []]
       (if (or (nil? path-group) (not (apply = path-group)))
         common-path
         (recur rest-path-groups (conj common-path (first path-group)))))
     var-deps]))

(defn remove-from-path [n form]
  {:post [(-> (:path %) empty? not)]}
  (update form :path subvec n))

(defn group-dynamic-forms [dynamic-forms]
  {:pre [(not (empty? dynamic-forms))]}
  (let [partitioned (partition-by (comp first :path) dynamic-forms)]
    (prn partitioned)
    (for [forms partitioned]
      (if (= 1 (count forms))
        (first forms)
        (let [[common-path var-deps] (compute-common-path forms)
              n (count common-path)
              updated-forms (map (partial remove-from-path n) forms)]
          {:path common-path :var-deps var-deps
           :sub-forms (group-dynamic-forms updated-forms)})))))

(comment
  (compute-common-path [{:path [2 0 0] :var-deps #{1}}
                {:path [2 0 1 0 0] :var-deps #{1 2}}
                        {:path [2 0 1 0 1]}])

  (group-dynamic-forms '[{:path [0 1] :var-deps #{x} :form x}
                         {:path [2 0 0] :var-deps #{x} :form x}
                         {:path [2 0 1 0 0] :var-deps #{x y} :form y}
                         {:path [2 0 1 0 1] :var-deps #{z} :form z}])

  ;; Should throw an error
  (group-dynamic-forms '[{:path [0 1] :var-deps #{x} :form x}
                         {:path [2 0 1] :var-deps #{x} :form x}
                         {:path [2 0 1 0 0] :var-deps #{x y} :form y}
                         {:path [2 0 1 0 1] :var-deps #{z} :form z}])
  )

(declare dynamic-forms->update-expr)

(defn dynamic-form->update-expr [{:keys [path var-deps form sub-forms]}]
  {:pre [(or (and form (nil? sub-forms))
             (and (nil? form) sub-forms))
         (not (empty? path))]}
  (if form
    `(update-in ~path (constantly ~form))
    `(update-in ~path ~(dynamic-forms->update-expr sub-forms))))

(defn dynamic-forms->update-expr [dynamic-forms]
  {:pre [(not (empty? dynamic-forms))]}
  (let [update-exprs (map dynamic-form->update-expr dynamic-forms)]
    `(fn [form#]
       (-> form# ~@update-exprs))))

(comment
  (dynamic-forms->update-expr
   '({:path [0 1] :var-deps #{x} :form x}
     {:path [2 0]
      :var-deps #{x y z}
      :sub-forms
      ({:path [0] :var-deps #{x} :form x}
       {:path [1 0]
        :var-deps #{x y z}
        :sub-forms
        ({:path [0] :var-deps #{x y} :form y}
         {:path [1] :var-deps #{z} :form z})})}))
  )

(defn extract-params [params]
  (let [extracted (atom [])]
    (postwalk (fn [x]
                (when (and (not= '& x) (symbol? x))
                  (swap! extracted conj x))
                x)
              params)
    @extracted))

(comment
  (extract-params '[{:e r :as rr} y & others])
  )

(defn compile-data [content]
  (let [[static dynamic] (binding [*dynamic-forms* []]
                           [(compile-element content []) *dynamic-forms*])
        _ (prn dynamic)
        cached-sym (gensym "cached")
        update-expr (when dynamic (dynamic->update-expr dynamic))
        ret `(let [~cached-sym (if comp/*cache*
                                 (get comp/*cache* ~*cache-counter*)
                                 ~static)
                   ~@(when update-expr
                       [cached-sym `(~update-expr ~cached-sym)])]
               ~(when *cache-counter*
                  `(when comp/*cache*
                     (set! comp/*cache*
                           (assoc
                            comp/*cache* ~*cache-counter* ~cached-sym))))
           ~cached-sym)]
    (when *cache-counter* (set! *cache-counter* (inc *cache-counter*)))
    ret))

(comment
  (require '[clojure.pprint :refer [pprint pp]])

  (binding [*dynamic-forms* #{}]
    (compile-element '[:e {} [:p {} x]] []))

  (binding [*cache-counter* 0]
    (let [x "c"]
      (compile-data '[:e {} [:p {} x]])))
 )
