(ns ewen.inccup.incremental.component)

(deftype Component [id static params forms]
  IDeref
  (-deref [_] forms))
