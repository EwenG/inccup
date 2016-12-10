(ns ewen.inccup.incremental.batching
  (:require [goog.object]))

(defn fake-raf [f]
  (js/setTimeout f 16))

(def next-tick
  (if (exists? js/window)
    (or (.-requestAnimationFrame js/window)
        (.-webkitRequestAnimationFrame js/window)
        (.-mozRequestAnimationFrame js/window)
        (.-msRequestAnimationFrame js/window)
        fake-raf)
    fake-raf))

(defn schedule [queue]
  (when-not (goog.object/get queue "inccup/scheduled")
    (goog.object/set queue "inccup/scheduled" true)
    (next-tick #())))

#_(defn process-queue [queue]
  (goog.object/set queue "inccup/scheduled" false)
  (let [length (.-length queue)]
    (loop [comp (.pop queue)
           index 0]
      (when (< index length)
        (when (goog.object/get comp "inccup/is-dirty")
          )
        (recur (inc index))))))
