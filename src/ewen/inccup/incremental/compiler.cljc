(ns ewen.inccup.incremental.compiler
  (:require [clojure.string :as str]))

(def ^:dynamic *cache* nil)

(def ^{:doc "Regular expression that parses a CSS-style id and class from
an element name."
       :private true}
  re-tag #"([^\s\.#]+)(?:#([^\s\.#]+))?(?:\.([^\s#]+))?")

(defn unevaluated?
  "True if the expression has not been evaluated."
  [expr]
  (or (symbol? expr)
      (and (seq? expr)
           (not= (first expr) `quote))))

(defn merge-attributes [attrs1 {:keys [id] :as attrs2}]
  (let [merged-attrs (merge-with
                      #(cond (nil? %1) %2
                             (unevaluated? %2) `(str ~%1 " " ~%2)
                             :else (str %1 " " %2))
                      attrs1 attrs2)]
    (if id (assoc merged-attrs :id id) merged-attrs)))

(defn normalize-element
  "Ensure an element vector is of the form [tag-name attrs content]."
  [[tag & content]]
  (when (not (or (keyword? tag) (symbol? tag) (string? tag)))
    #?(:clj (throw (IllegalArgumentException.
                    (str tag " is not a valid element name.")))
       :cljs (throw (js/Error. (str tag " is not a valid element name.")))))
  (let [[_ tag id class] (re-matches re-tag (name tag))
        tag-attrs        (cond-> {}
                           id (assoc :id id)
                           class (assoc
                                  :class
                                  (if class
                                    (str/replace ^String class "." " "))))
        map-attrs        (first content)]
    (if (map? map-attrs)
      [tag (merge-attributes tag-attrs map-attrs) (next content)]
      [tag tag-attrs content])))

(defn merge-shortcut-attributes [attrs1 attrs2]
  (let [attrs1-meta (meta attrs1)
        map-attrs (::map-attrs attrs1-meta)
        merged-attrs (merge-with
                      #(cond (nil? %1) %2
                             (unevaluated? %2) `(str ~%1 " " ~%2)
                             :else (str %1 " " %2))
                      map-attrs attrs2)]
    (with-meta merged-attrs (assoc attrs1-meta ::shortcut-attrs attrs2))))

(defn merge-map-attributes [attrs1 {:keys [id] :as attrs2}]
  (let [attrs1-meta (meta attrs1)
        shortcut-attrs (::shortcut-attrs attrs1-meta)
        merged-attrs (merge-with
                      #(cond (nil? %1) %2
                             (unevaluated? %2) `(str ~%1 " " ~%2)
                             :else (str %1 " " %2))
                      shortcut-attrs attrs2)
        merged-attrs (if id (assoc merged-attrs :id id) merged-attrs)]
    (with-meta merged-attrs (assoc attrs1-meta ::map-attrs attrs2))))

#?(:cljs
   (defn safe-aset [obj k v]
     (when obj (aset obj k v))))

#?(:cljs
   (defn safe-aget [obj k]
     (when obj (aget obj k))))

#?(:cljs
   (defn init-cache []
     (js-obj "dynamic-counter" 0
             "dynamic-array" (array))))

#?(:cljs
   (defn new-dynamic-cache [cache]
     (when cache
       (let [dynamic-counter (aget cache "dynamic-counter")
             current-cache (-> (aget cache "dynamic-array")
                               (aget dynamic-counter))]
         (aset cache "dynamic-counter" (inc dynamic-counter))
         (or current-cache
             (let [new-cache (js-obj "sub-cache" (array))]
               (.push (aget cache "dynamic-array") new-cache)
               new-cache))))))

#?(:cljs
   (defn clean-dynamic-array [c]
     (let [dyn-arr (aget c "dynamic-array")
           dyn-counter (aget c "dynamic-counter")
           cache-shrink-nb (- (count dyn-arr) dyn-counter)]
       ;; dyn counter can be 0 either because the sub-component was not
       ;; called because the var(s) it depends on did not change or because
       ;; it is inside a loop that was executed on an empty collection.
       ;; In the first case, we must not clean the dynamic array.
       ;; The fact that we don't clean the array in the second case is an
       ;; unfortunate side effect, but I can't see a simple way to avoid it.
       (when (not= dyn-counter 0)
         (dotimes [_ cache-shrink-nb]
           (.pop dyn-arr))
         (aset c "dynamic-counter" 0)))))

#?(:cljs
   (defn clean-sub-cache [cache]
     (when cache
       (doseq [c (aget cache "sub-cache")]
         (clean-dynamic-array c)))))

#?(:cljs
   (defn get-static-cache [cache static-counter]
     (when cache
       (-> (aget cache "sub-cache")
           (aget static-counter)))))

#?(:cljs
   (defn make-static-cache [cache static-counter]
     (when cache
       (let [sub-cache (aget cache "sub-cache")]
         (when (not= static-counter (count sub-cache))
           (dotimes [_ static-counter]
             (.push sub-cache (init-cache))))))))

(comment
  (merge-shortcut-attributes (with-meta {:f "e"}
                               {::map-attrs {:g "g"}})
                             {:e "e"})

  (meta (merge-map-attributes (with-meta {:f "e"}
                                {::shortcut-attrs {:g "g"}})
                              {:e "e"}))
  )
