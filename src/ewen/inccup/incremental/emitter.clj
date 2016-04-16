(ns ewen.inccup.incremental.emitter
  (:require [cljs.analyzer.api :as ana-api]
            [clojure.tools.reader :as reader]
            [cljs.analyzer :as ana]
            [cljs.tagged-literals :as tags]
            [cljs.env :as env]))

(defmulti emit* :op)

(defmethod emit* :vector
  [{:keys [items form] :as ast}]
  (apply vector (map emit* items)))

(defmethod emit* :constant
  [{:keys [form env]}] form)

(defmethod emit* :map
  [{:keys [keys vals]}]
  (zipmap (map emit* keys)
          (map emit* vals)))

(defmethod emit* :list
  [{:keys [items]}] (map emit* items))

(defmethod emit* :set
  [{:keys [items]}] (into #{} (map emit* items)))

(defmethod emit* :var-special
  [{{{name :name} :info} :var}] `(var ~name))

(defmethod emit* :def
  [{:keys [var init]}] `(def ~(emit* var) ~(emit* init)))

(defn var-emit [{:keys [info env form] :as ast}]
  (:name info))

(defmethod emit* :do
  [{:keys [statements ret]}]
  `(do ~@(into (list (emit* ret)) (map emit* statements))))

(defn emit-do-content [{:keys [statements ret]}]
  (into (list (emit* ret)) (map emit* statements)))

(defn invoke-emit [{:keys [f args] :as ast}]
  (conj (map emit* args) (emit* f)))

(defmethod emit* :let
  [{:keys [bindings expr] :as ast}]
  `(~'let ~(->> (for [{:keys [init] :as binding} bindings]
                  [(emit* binding) (emit* init)])
                (apply concat)
                vec)
     ~@(emit-do-content expr)))

(defn emit-fn-params [params variadic]
  (if variadic
    (-> (mapv emit* (butlast params))
        (conj '& (emit* (last params))))
    (mapv emit* params)))

(defmethod emit* :fn
  [{:keys [variadic name methods form] :as ast}]
  (if (= 1 (count methods))
    (let [{:keys [params expr variadic]} (first methods)]
      `(~'fn ~@(if name (list (:name name)) nil)
         ~(emit-fn-params params variadic)
         ~@(emit-do-content expr)))
    (let [params (map #(emit-fn-params (:params %) (:variadic %)) methods)
          bodys (mapcat #(emit-do-content (:expr %)) methods)]
      `(~'fn ~@(if name (list (:name name)) nil)
         ~@(->> (interleave params bodys)
                (partition 2))))))

(defn emit-fn-content
  [{:keys [variadic name methods form] :as ast}]
  (if (= 1 (count methods))
    (let [{:keys [params expr variadic]} (first methods)]
      `(~@(if name (list (:name name)) nil)
         ~(emit-fn-params params variadic)
         ~@(emit-do-content expr)))
    (let [params (map #(emit-fn-params (:params %) (:variadic %)) methods)
          bodys (mapcat #(emit-do-content (:expr %)) methods)]
      `(~@(if name (list (:name name)) nil)
         ~@(->> (interleave params bodys)
                (partition 2))))))

(defmethod emit* :js
  [{:keys [code form args] :as ast}]
  (if code
    `(~'js* ~code)
    `(~'js* ~@(conj (doall (map emit* args)) (second form)))))

(defmethod emit* :if
  [{:keys [test then else] :as ast}]
  `(if ~(emit* test) ~(emit* then) ~(emit* else)))

(defmethod emit* :new
  [{:keys [ctor args] :as ast}]
  `(~(symbol (str (emit* ctor) "."))
    ~@(map emit* args)))

(defmethod emit* :loop
  [{:keys [bindings expr] :as ast}]
  `(~'loop ~(->> (for [{:keys [init] :as binding} bindings]
                  [(emit* binding) (emit* init)])
                (apply concat)
                vec)
     ~@(emit-do-content expr)))

(defmethod emit* :recur
  [{:keys [exprs] :as ast}]
  `(recur ~@(map emit* exprs)))

(defmethod emit* :letfn
  [{:keys [bindings expr] :as ast}]
  `(~'letfn ~(->> (for [{:keys [init]} bindings]
                   [(emit-fn-content init)])
                 (apply concat)
                 vec)
     ~@(emit-do-content expr)))

(defmethod emit* :no-op [ast]
  nil)

(defmethod emit* :dot
  [{:keys [target method args] :as ast}]
  `(~(symbol (str "." method)) ~(emit* target) ~@(map emit* args)))

(defmethod emit* :throw
  [{:keys [throw] :as ast}]
  `(throw ~(emit* throw)))

(defmethod emit* :try
  [{:keys [try catch name throw finally] :as ast}]
  (cond (and name finally)
    `(try ~(emit* try)
          (catch :default ~name ~(emit* catch))
          (finally ~(emit* finally)))
    name
    `(try ~(emit* try)
          (catch :default ~name ~(emit* catch)))
    finally
    `(try ~(emit* try)
          (finally ~(emit* finally)))
    :else
    `(try ~(emit* try))))

(comment

  (emit* (ana-api/analyze
          (ana-api/empty-env)
          '(try "e"
                (catch :default e "e")
                (finally "r"))))

  )

(defmethod emit* :set!
  [{:keys [target val] :as ast}]
  `(~'set! ~(emit* target) ~(emit* val)))

(defmethod emit* :case*
  [{:keys [v tests thens default] :as ast}]
  `(~'case*
    ~(emit* v)
    ~(mapv (fn [tests-inner]
             (mapv emit* tests-inner))
           tests)
    ~(mapv emit* thens)
    ~(emit* default)))

(comment
  (emit* (ana-api/analyze
          (ana-api/empty-env)
          '(case e (1 2) "1" 3 "3" "d")))
  )

(defmethod emit* :meta
  [{:keys [expr meta form] :as ast}]
  `(with-meta ~(emit* expr) ~(emit* meta)))

(comment
  (emit* (ana-api/analyze
          (ana-api/empty-env)
          [(with-meta {} {:e "e"})]))
  )

(defmethod emit* :deftype*
  [{:keys [form t fields pmasks protocols body] :as ast}]
  (throw (Exception. "deftype is not support by inccup")))

(defmethod emit* :defrecord*
  [{:keys [form] :as ast}]
  (throw (Exception. "defrecord is not supported by inccup")))

(defmethod emit* :ns
  [{:keys [form] :as ast}]
  (throw (Exception. "ns is not supported by inccup")))

(defmethod emit* :js-value
  [{:keys [form] :as ast}]
  (throw (Exception. "js-value is not supported by inccup")))


(comment
  (require '[cljs.compiler :as cljs-comp])
  (keys (methods cljs-comp/emit*))

  (require '[clojure.pprint :refer [pprint pp]])
  (defn ast-show-only [ast keys]
    (assoc (select-keys ast keys)
           :children (mapv #(ast-show-only % keys) (:children ast))))

  (emit*
   (ana-api/analyze
    (ana-api/empty-env)
    '(def e "e")))

  (emit*
   (ana-api/analyze    (ana-api/empty-env)
    '(let [e "e"]
       e)))

  (ast-show-only (ana-api/analyze (ana-api/empty-env)
                           '(let [e "e"]
                              (ewen.inccup.incremental.emitter/mm))) [:op])

  (get-in
   (ana-api/analyze
    (ana-api/empty-env)
    '(fn [e]
       (fn [e]
         e))) [:children 0 :children 0 :children 0 :children 0])

  (emit* (ana-api/analyze
                  (ana-api/empty-env)
                  '(js*
                    "[cljs.core.str(~{}),cljs.core.str(~{}),cljs.core.str(~{})].join('')"
                    "a"
                    " "
                    x)))

  (:segs (ana-api/analyze
          (ana-api/empty-env)
          '(str "e" "f")))

  (defmacro mm [] (prn "e") :e)

  (emit* (ana-api/analyze
          (ana-api/empty-env)
          '(for [x ["e"]]
             (ewen.inccup.incremental.emitter/mm))))

  )
