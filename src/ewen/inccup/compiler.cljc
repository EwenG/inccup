(ns ewen.inccup.compiler)

(defprotocol HtmlRenderer
  (render-html [this]))

(defn render-element
  [element]
  (let [[tag attrs content] (normalize-element element)
        content (render-html content)]
    (if (seq? content)
      (into [tag (render-attr-map attrs)] (render-html content))
      [tag (render-attr-map attrs) (render-html content)])))

(extend-protocol HtmlRenderer
  #?(:clj IPersistentVector)
  #?(:cljs IVector)
  (render-html [this]
    (render-element this))
  ISeq
  (render-html [this]
    (map render-html this))
  INamed
  (render-html [this]
    (name this))
  object
  (render-html [this]
    (str this))
  nil
  (render-html [this]
    '()))
