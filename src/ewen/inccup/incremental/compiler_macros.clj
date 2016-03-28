(ns ewen.inccup.incremental.compiler-macros
  (:require [ewen.inccup.incremental.compiler :as comp]
            [ewen.inccup.incremental.emitter :as emitter
             :refer [*tracked-vars* track-vars]]
            [cljs.analyzer.api :refer [analyze empty-env]]
            [clojure.walk :refer [postwalk]]
            [clojure.set :refer [union]]))

(def ^:dynamic *cache-sym* nil)
(def ^:dynamic *cache-counter* nil)
(def ^:dynamic *params-changed-sym* nil)
(def ^:dynamic *dynamic-forms* nil)

(defn update-dynamic-forms
  ([expr path used-vars]
   (update-dynamic-forms expr path used-vars nil))
  ([expr path used-vars predicate]
   (when *dynamic-forms*
     (let [{prev-path :path
            prev-var-deps :var-deps
            prev-form :form
            prev-predicate :predicates} (peek *dynamic-forms*)]
       (if (= path prev-path)
         (set! *dynamic-forms*
               (conj (pop *dynamic-forms*)
                     {:path path
                      :var-deps (union used-vars prev-var-deps)
                      :form (comp expr prev-form)
                      :predicates (union ~predicate ~prev-predicate)}))
         (set! *dynamic-forms*
               (conj *dynamic-forms*
                     {:path path
                      :var-deps used-vars
                      :form expr
                      :predicates (if predicate #{predicate} nil)})))))))

(defn compile-dynamic-expr [expr path]
  (let [[expr used-vars] (track-vars expr)
        expr `(constantly ~expr)]
    (update-dynamic-forms expr path used-vars)
    (if (empty? used-vars) expr nil)))

(def ^{:doc "Regular expression that parses a CSS-style id and class from
an element name."
       :private true}
  re-tag #"([^\s\.#]+)(?:#([^\s\.#]+))?(?:\.([^\s#]+))?")

(def ^{:doc "A list of elements that must be rendered without a closing
tag."
       :private true}
  void-tags
  #{"area" "base" "br" "col" "command" "embed" "hr" "img" "input" "keygen"
    "link" "meta" "param" "source" "track" "wbr"})

(defn- container-tag?
  "Returns true if the tag has content or is not a void tag."
  [tag content]
  (or content (not (void-tags tag))))

(defn normalize-element
  "Ensure an element vector is of the form [tag-name attrs content]."
  [[tag & content]]
  (when (not (or (keyword? tag) (symbol? tag) (string? tag)))
    (throw (IllegalArgumentException.
            (str tag " is not a valid element name."))))
  (let [[_ tag id class] (re-matches re-tag (name tag))
        tag-attrs        (cond-> {}
                           id (assoc :id id)
                           class (assoc
                                  :class
                                  (if class
                                    (.replace ^String class "." " "))))
        map-attrs        (first content)]
    (if (map? map-attrs)
      [tag (comp/merge-attributes tag-attrs map-attrs) (next content)]
      [tag tag-attrs content])))

(defn compile-attr-map
  "Returns an unevaluated form that will render the supplied map as HTML
  attributes."
  [attrs path]
  (if (some comp/unevaluated? (mapcat identity attrs))
    (compile-dynamic-expr attrs path)
    attrs))

(defn maybe-attr-map
  ([attrs attr-path]
   (maybe-attr-map attrs attr-path nil))
  ([attrs attr-path path]
   {:pred [(not (some comp/unevaluated? (mapcat identity attrs)))]}
   (when attrs
     (let [[expr used-vars] (track-vars attrs)
           attr-form `#(comp/merge-attributes % ~expr)
           form (when path `(constantly ~expr))]
       (update-dynamic-forms attr-form attr-path used-vars `(map? ~expr))
       (when expr (update-dynamic-forms
                   form path used-vars `(not (map? ~expr))))
       nil))))

(defn- form-name
  "Get the name of the supplied form."
  [form]
  (if (and (seq? form) (symbol? (first form)))
    (name (first form))))

(declare compile-html)

(defmulti compile-form
  "Pre-compile certain standard forms, where possible."
  {:private true}
  form-name)

(defmethod compile-form :default
  [expr path] (compile-dynamic-expr expr path))

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
  (and (not (comp/unevaluated? x))
       (or (not (or (vector? x) (map? x)))
           (every? literal? x))))

(defn- not-implicit-map?
  "True if we can infer that x is not a map."
  [x]
  (or (= (form-name x) "for")
      (not (comp/unevaluated? x))
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
  (let [[tag attrs _] (normalize-element [tag attrs])
        compiled-attrs (compile-attr-map attrs (conj path 1))]
    (if (container-tag? tag content)
      (into [tag compiled-attrs] (compile-seq content path 2))
      [tag compiled-attrs])))

(defmethod compile-element ::literal-tag-and-no-attributes
  [[tag & content] path]
  (compile-element (apply vector tag {} content) path))

(defmethod compile-element ::literal-tag
  [[tag attrs & content :as element] path]
  (let [[tag tag-attrs [first-content & rest-content]]
        (normalize-element element)]
    (if (container-tag? tag content)
      (do
        (maybe-attr-map first-content (conj path 1) (conj path 2))
        (into [tag tag-attrs nil] (compile-seq rest-content path 3)))
      (do
        (maybe-attr-map first-content (conj path 1))
        (into [tag tag-attrs])))))

(declare compile-dispatch)

(defn- compile-seq
  "Compile a sequence of data-structures into HTML."
  ([content path]
   (compile-seq content path 0))
  ([content path index-init]
   (loop [[expr & rest-content] content
          index index-init
          compiled []]
     (if expr
       (let [compiled-expr (compile-dispatch expr (conj path index))]
         (recur rest-content (inc index) (conj compiled compiled-expr)))
       compiled))))

(defn- compile-dispatch [expr path]
  (cond
    (vector? expr) (compile-element expr path)
    (string? expr) expr
    (keyword? expr) expr
    (literal? expr) expr
    (seq? expr) (compile-form expr path)
    :else (compile-dynamic-expr expr path)))

(defn compute-common-path [dynamic-forms]
  (let [var-deps (apply union (map :var-deps dynamic-forms))
        predicates (apply union (map :predicates dynamic-forms))
        path-groups (->> (map :path dynamic-forms)
                         (apply interleave)
                         (partition (count dynamic-forms)))
        common-path (loop [[path-group & rest-path-groups] path-groups
                           common-path []]
                      (if (or (nil? path-group) (not (apply = path-group)))
                        common-path
                        (recur rest-path-groups
                               (conj common-path (first path-group)))))]
    [common-path var-deps predicates]))

(defn remove-from-path [n form]
  {:post [(-> (:path %) empty? not)]}
  (update form :path subvec n))

(defn group-dynamic-forms [dynamic-forms]
  {:pre [(not (empty? dynamic-forms))]}
  (let [partitioned (partition-by (comp first :path) dynamic-forms)]
    (for [forms partitioned]
      (if (= 1 (count forms))
        (first forms)
        (let [[common-path var-deps predicates] (compute-common-path forms)
              n (count common-path)
              updated-forms (map (partial remove-from-path n) forms)]
          {:path common-path :var-deps var-deps :predicates predicates
           :sub-forms (group-dynamic-forms updated-forms)})))))

(comment
  (compute-common-path [{:path [2 0 0]
                         :var-deps #{1}
                         :predicates #{'a}}
                        {:path [2 0 1 0 0]
                         :var-deps #{1 2}
                         :predicates #{'a 'b 'c}}
                        {:path [2 0 1 0 1]
                         :var-deps #{1 2}
                         :predicates nil}])

  (group-dynamic-forms '[{:path [0 1]
                          :var-deps #{x}
                          :form (constantly x)
                          :predicates #{'a}}
                         {:path [2 0 0]
                          :var-deps #{x}
                          :form (constantly x)
                          :predicates #{'a 'b}}
                         {:path [2 0 1 0 0]
                          :var-deps #{x y}
                          :form #(update-in % (constantly y))
                          :predicates #{}}
                         {:path [2 0 1 0 1]
                          :var-deps #{z}
                          :form (constantly z)
                          :predicates #{'b 'c}}])

  ;; Should throw an error
  (group-dynamic-forms '[{:path [0 1] :var-deps #{x} :form (constantly x)}
                         {:path [2 0 1] :var-deps #{x} :form (constantly x)}
                         {:path [2 0 1 0 0]
                          :var-deps #{x y}
                          :form (constantly y)}
                         {:path [2 0 1 0 1]
                          :var-deps #{z}
                          :form (constantly z)}])
  )

(declare dynamic-forms->update-expr)

(defn format-predicates [predicates]
  (cond (empty? predicates) true
        (= 1 (count predicates)) (first predicates)
        :else `(and ~@predicates)))

(defn var-deps->predicate [var-deps]
  (cond (nil? *params-changed-sym*) true
        (empty? var-deps) false
        :else (let [preds (doall
                           (map #(get *params-changed-sym* %) var-deps))]
                (if (= 1 (count preds))
                  (first preds)
                  `(or ~@preds)))))

(defn dynamic-form->predicate [var-deps predicates]
  (let [predicates (format-predicates predicates)
        var-deps-p (var-deps->predicate var-deps)]
    (cond (true? var-deps-p) predicates
          (true? predicates) var-deps-p
          (false? var-deps-p) false
          (false? predicates) false
          :else `(and ~var-deps-p ~predicates))))

(defn dynamic-form->update-expr [{:keys [path var-deps form
                                         sub-forms predicates]}]
  {:pre [(or (and form (nil? sub-forms))
             (and (nil? form) sub-forms))
         (not (empty? path))]}
  (if form
    `(~(dynamic-form->predicate var-deps predicates)
      (update-in ~path ~form))
    `(~(dynamic-form->predicate var-deps predicates)
      (update-in ~path ~(dynamic-forms->update-expr sub-forms)))))

(defn dynamic-forms->update-expr [dynamic-forms]
  (cond (empty? dynamic-forms) ;; only static
        `identity
        (= [] (:path (first dynamic-forms))) ;; only dynamic
        (do
          (assert (= 1 (count dynamic-forms)))
          `(constantly ~(:form (first dynamic-forms))))
        :else
        (let [update-exprs (mapcat dynamic-form->update-expr dynamic-forms)]
          `(fn [form#]
             (cond-> form# ~@update-exprs)))))

(comment
  (binding [*params-changed-sym* {'x 'x123 'y 'y234 'z 'z456}]
    (dynamic-forms->update-expr
     '({:path [0 1] :var-deps #{x} :form x}
       {:path [2 0]
        :var-deps #{x y z}
        :predicates #{'ppp1 'ppp2 'ppp3}
        :sub-forms
        ({:path [0] :var-deps #{x} :predicates #{'ppp1} :form x}
         {:path [1 0]
          :var-deps #{x y z}
          :predicates #{'ppp2 'ppp3}
          :sub-forms
          ({:path [0] :var-deps #{x y} :predicates #{'ppp2 'ppp3} :form y}
           {:path [1] :var-deps #{z} :form z})})})))
  )

(defn extract-params [params]
  (let [extracted (atom #{})]
    (postwalk (fn [x]
                (when (and (not= '& x) (symbol? x))
                  (swap! extracted conj x))
                x)
              params)
    @extracted))

(comment
  (extract-params '[{:e r :as rr} y & others])
  )

(defn compile-inc* [content env]
  (let [[static dynamic]
        (binding [emitter/*env* env
                  *dynamic-forms* []
                  *cache-counter* (when *cache-counter*
                                    (inc *cache-counter*))]
          [(compile-dispatch content []) *dynamic-forms*])
        update-expr (dynamic-forms->update-expr dynamic)
        cached-sym (gensym "cached")]
    `(let [~cached-sym (or (get @~*cache-sym* ~*cache-counter*) ~static)
           ~cached-sym (~update-expr ~cached-sym)]
       ~(when *cache-counter*
          `(when ~*cache-sym*
             (swap! ~*cache-sym*
                    assoc ~*cache-counter* ~cached-sym)))
       ~cached-sym)))

(defn with-params-changed [params params-changed-sym cache-sym & body]
  (let [changed-bindings
        (mapcat (fn [param]
                  [(get params-changed-sym param)
                   `(or (not (:params @~cache-sym))
                        (not= ~param (get (:params @~cache-sym) '~param)))])
                params)]
    `(let ~(vec changed-bindings) ~@body)))

(defmacro compile-inc
  ([content] (compile-inc* content &env))
  ([content params cache-sym]
   (let [params-with-sym (interleave (map #(list 'quote %) params) params)
         params-changed-sym (zipmap params (map #(gensym (name %)) params))
         tracked-vars
         (loop [tracked-vars {}
                params params]
           (if-let [param (first params)]
             (do (assert (contains? (:locals &env) param))
                 (recur (assoc tracked-vars param
                               {:env (get (:locals &env) param)
                                :is-used false
                                :symbol param})
                        (rest params)))
             tracked-vars))]
     (binding [*tracked-vars* tracked-vars
               *params-changed-sym* params-changed-sym
               *cache-counter* 0
               *cache-sym* cache-sym]
       (with-params-changed params params-changed-sym cache-sym
         `(swap! ~*cache-sym* assoc :params
                 ~(conj params-with-sym `hash-map))
         (compile-inc* content &env))))))

(comment
  (require '[clojure.pprint :refer [pprint pp]])

  (binding [*dynamic-forms* #{}]
    (compile-element '[:e {} [:p {} x]] []))

  (binding [*cache-counter* 0]
    (let [x "c"]
      (compile-inc '[:e {} [:p {} x]])))
 )
