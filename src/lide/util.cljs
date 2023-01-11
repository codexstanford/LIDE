(ns lide.util
  (:require
   [clojure.string :as string]))

;; General utilities

(defn map-keys
  "Apply `f` to keys in `m`, keeping the same values."
  [f m]
  (->> m
       (map
        (fn [[k v]]
          [(f k) v]))
       (into {})))

(defn map-vals
  "Apply `f` to values in `m`, keeping the same keys."
  [f m]
  (into (empty m) (for [[k v] m] [k (f v)])))

;; DOM transform matrix stuff

(defn dom-matrix-to-vals [dm]
  [(.-a dm)
   (.-b dm)
   (.-c dm)
   (.-d dm)
   (.-e dm)
   (.-f dm)])

(defn dom-matrix-from-vals [vals]
  (js/DOMMatrix. vals))

;; Rule/program utilities

(defn populate-rule
  "Retrieve the actual values from `program` pointed to by the IDs in `rule`."
  [program rule]
  (-> rule
      (update :head (fn [id]
                      (-> program :literals (get id))))
      (update :body (fn [literals]
                      (mapv (fn [id]
                              (assoc (-> program :literals (get id))
                                     :id id))
                            literals)))))

(defn parse-body-arg
  "Parse an argument to a body literal. Some special formats are allowed in this
  context: `arg` might be more than a plain string."
  [arg]
  (let [unparsable {:type :uncontrolled
                    :value arg}]
    (cond
      (string/blank? arg) unparsable ;; redundant with :else but a useful guard

      (> (count (string/split arg "->")) 1)
      {:type :attribute-path
       :value (string/split arg "->")}

      :else unparsable)))
