(ns lide.util)

(defn dom-matrix-to-vals [dm]
  [(.-a dm)
   (.-b dm)
   (.-c dm)
   (.-d dm)
   (.-e dm)
   (.-f dm)])

(defn dom-matrix-from-vals [vals]
  (js/DOMMatrix. vals))

(defn map-vals [f m]
  (into (empty m) (for [[k v] m] [k (f v)])))
