(ns lide.util
  (:require
   [clojure.string :as string]))

;; General utilities

(defn map-vals [f m]
  (into (empty m) (for [[k v] m] [k (f v)])))

(defn first-indexed [pred coll]
  "Return a vector of the first element in `coll` matching `pred` and the index
  of that element, ordered [index, item]."
  (->> coll
       (keep-indexed (fn [idx elem]
                       (when (pred elem)
                         [idx elem])))
       first))

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

(defn populate-rule [program rule]
  (-> rule
      (update :head (fn [id]
                      (-> program :literals (get id))))
      (update :body (fn [literals]
                      (mapv (fn [id] (-> program :literals (get id)))
                            literals)))))

(defn variable? [arg]
  "True if `arg` is a string starting with an upper-case letter."
  (let [first-char (subs arg 0 1)
        first-char-alpha (apply str (re-seq #"[a-zA-Z]" first-char))]
    (= first-char
       first-char-alpha
       (string/upper-case first-char))))

(defn ground? [arg]
  (not (variable? arg)))

(defn grounds? [a b]
  "Literal `b` grounds literal `a` if they have the same predicate, same arity,
  and at least one shared argument is ground in `b` and variable in `a`. "
  (and (= (:predicate a)
          (:predicate b))
       (= (count (:args a))
          (count (:args b)))
       (some (fn [[a-arg b-arg]]
               (and (variable? a-arg)
                    (ground? b-arg)))
             (map vector (:args a) (:args b)))))

(defn all-body-literals [program]
  (->> (:rules program)
       (map
        (fn [ref-rule]
          (map
           (fn [body-literal-id]
             {body-literal-id (get (:literals program) body-literal-id)})
           (:body ref-rule))))
       flatten
       (into {})))

(defn compositions [program rule]
  (->> (:body rule)
       (map
        (fn [body-literal-id]
          (let [body-literal (get (:literals program) body-literal-id)]
            (mapv
             (fn [arg]
               (when (and (not= arg :unspecified)
                          ;; Ground args aren't relevant here because there's
                          ;; no unification to be done with them
                          (not (ground? arg)))
                 {:literal-id body-literal-id
                  :arg        arg}))
             (:args body-literal)))))
       flatten
       (remove nil?)))

(defn all-compositions [program]
  (->> (:rules program)
       (map-indexed
        (fn [rule-idx rule]
          (mapv
           (fn [composition]
             )
           (compositions rule-idx rule))))))
