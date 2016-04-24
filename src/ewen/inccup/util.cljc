(ns ewen.inccup.util
  "Utility functions for Hiccup."
  (:require [clojure.string :as str])
  #?(:clj (:import [clojure.lang Keyword]
                   [java.net URI]
                   [java.net URLEncoder])
     :cljs (:import [goog Uri])))

(def ^:dynamic *html-mode* :xhtml)

(def ^:dynamic *base-url* nil)

(def ^:dynamic *is-top-level* true)

;; Used in the \"html\" macro and thus only relevant on the :clj platform.
;; One must use a side effectful macro in order to modify the root value
#?(:clj (def ^:dynamic *output-format* nil))

#?(:clj (defn cljs-env?
          "Take the &env from a macro, and tell whether we are expanding
  into cljs."
          [env]
          (boolean (:ns env))))

#?(:clj (defn default-output-format [env]
          (if (cljs-env? env) :inccup :string)))

(defprotocol ToString
  #?(:clj (^String to-str [x] "Convert a value into a string.")
     :cljs (to-str [x] "Convert a value into a string.")))

(extend-protocol ToString
  Keyword
  (to-str [k] (name k))
  #?@(:clj [clojure.lang.Ratio
            (to-str [r] (str (float r)))])
  #?@(:clj [java.net.URI
            (to-str [u]
                    (if (or (.getHost u)
                            (nil? (.getPath u))
                            (not (-> (.getPath u) (.startsWith "/"))))
                      (str u)
                      (let [base (str *base-url*)]
                        (if (.endsWith base "/")
                          (str (subs base 0 (dec (count base))) u)
                          (str base u)))))]
       :cljs [Uri
              (to-str [u]
                      (if (or (.hasDomain u)
                              (not (.hasPath u))
                              (not (-> (.getPath u)
                                       (goog.string.startsWith "/"))))
                        (str u)
                        (let [base (str *base-url*)]
                          (if (goog.string.endsWith base "/")
                            (str (subs base 0 (dec (count base))) u)
                            (str base u)))))])
  #?@(:cljs [string
             (to-str [x] (str x))
             number
             (to-str [x] (str x))])
  #?(:clj Object
     :cljs object)
  (to-str [x] (str x))
  nil
  (to-str [_] ""))

(defn ^String as-str
  "Converts its arguments into a string using to-str."
  [& xs]
  (apply str (map to-str xs)))

#?(:clj (defprotocol ToURI
          (^java.net.URI to-uri [x] "Convert a value into a URI."))
   :cljs (defprotocol ToURI
           (to-uri [x] "Convert a value into a URI.")))

(extend-protocol ToURI
  #?@(:clj [java.net.URI
            (to-uri [u] u)
            String
            (to-uri [s] (URI. s))]
       :cljs [Uri
              (to-uri [s] (Uri.parse s))
              string
              (to-uri [s] (Uri.parse s))
              object
              (to-uri [s]
                      (if (instance? js/String s)
                        (Uri.parse s)
                        (throw
                         (js/Error.
                          (str "Cannot convert object " s " to URI")))))]))

#?(:cljs (extend-type Uri
           IComparable
           (-compare [this other]
             (if (instance? Uri other)
               (-compare (str this) (str other))
               (throw
                (js/Error. (str "Cannot compare " this " to " other)))))
           IEquiv
           (-equiv [this  other]
             (if (instance? Uri other)
               (= (str this) (str other)) false))))

(deftype RawString [^String s]
  Object
  (^String toString [this] s))

(defn raw-string
  "Wraps a string to an object that will be pasted to HTML without
  escaping"
  ([] (RawString. ""))
  ([x] (RawString. x))
  ([x & xs] (RawString. (apply str x xs))))

(defn raw-string?
  "Returns true if x is a RawString"
  [x] (instance? RawString x))

(defn escape-html
  "Change special characters into HTML character entities."
  [text]
  (-> ^String (as-str text)
      (str/replace "&"  "&amp;")
      (str/replace "<"  "&lt;")
      (str/replace ">"  "&gt;")
      (str/replace "\"" "&quot;")
      (str/replace "'" (if (= *html-mode* :sgml) "&#39;" "&apos;"))))

(def ^{:doc "A set that compare objects by identity instead of
equality of value. The sort order is not relevant."}
  identity-set
  (sorted-set-by #(if (identical? %1 %2) 0 -1)))


(def ^:dynamic *encoding* "UTF-8")

(defprotocol URLEncode
  (url-encode [x] "Turn a value into a URL-encoded string."))

(extend-protocol URLEncode
  #?(:clj String :cljs string)
  (url-encode [s] #?(:clj (URLEncoder/encode s *encoding*)
                     :cljs (if (not= *encoding* "UTF-8")
                             (throw (js/Error. "Hiccup on Clojurescript only support UTF-8 encoding."))
                             (-> (js/encodeURIComponent s)
                                 (str/replace "%20" "+")))))
  #?(:clj java.util.Map :cljs PersistentArrayMap)
  (url-encode [m]
    (str/join "&"
              (for [[k v] m]
                (str (url-encode k) "=" (url-encode v)))))
  #?@(:cljs [number
             (url-encode [x] (str x))])
  #?(:clj Object
     :cljs object)
  (url-encode [x] (url-encode (to-str x))))

(defn url
  "Creates a URI instance from a variable list of arguments and an optional
  parameter map as the last argument. For example:
    (url \"/group/\" 4 \"/products\" {:page 9})
    => \"/group/4/products?page=9\""
  [& args]
  (let [params (last args), args (butlast args)]
    (to-uri
     (str (apply str args)
          (if (map? params)
            (str "?" (url-encode params))
            params)))))

#?(:clj
   (defmacro with-base-url
     "Sets a base URL that will be prepended onto relative URIs. Note that for this
  to work correctly, it needs to be placed outside the html macro."
     [base-url & body]
     `(binding [hiccup.util/*base-url* ~base-url]
        ~@body)))

#?(:clj
   (defmacro with-encoding
     "Sets a default encoding for URL encoding strings. Defaults to UTF-8."
     [encoding & body]
     `(binding [hiccup.util/*encoding* ~encoding]
        ~@body)))

(def ^{:doc "Regular expression that parses a CSS-style id and class
from an element name."
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
       :cljs (throw (js/Error.
                     (str tag " is not a valid element name.")))))
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

#?(:clj
   (defn cljs-env?
     "Take the &env from a macro, and tell whether we are expanding into
  cljs."
     [env]
     (boolean (:ns env))))

;; Taken from tools.macro
;; https://github.com/clojure/tools.macro
#?(:clj
   (defn name-with-attributes
     "To be used in macro definitions.
   Handles optional docstrings and attribute maps for a name to be defined
   in a list of macro arguments. If the first macro argument is a string,
   it is added as a docstring to name and removed from the macro argument
   list. If afterwards the first macro argument is a map, its entries are
   added to the name's metadata map and the map is removed from the
   macro argument list. The return value is a vector containing the name
   with its extended metadata map and the list of unprocessed macro
   arguments."
     [name macro-args]
     (let [[docstring macro-args]
           (if (string? (first macro-args))
             [(first macro-args) (next macro-args)]
             [nil macro-args])
           [attr macro-args]
           (if (map? (first macro-args))
             [(first macro-args) (next macro-args)]
             [{} macro-args])
           attr
           (if docstring
             (assoc attr :doc docstring)
             attr)
           attr
           (if (meta name)
             (conj (meta name) attr)
             attr)]
       [(with-meta name attr) macro-args])))
