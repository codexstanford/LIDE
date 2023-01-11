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

(defn first-indexed
  "Return a vector of the first element in `coll` matching `pred` and the index
  of that element, ordered [index, item]."
  [pred coll]
  (->> coll
       (keep-indexed (fn [idx elem]
                       (when (pred elem)
                         [idx elem])))
       first))

(defn vector-remove
  "Remove the element at `idx` from `vec`."
  [vec idx]
  (into (subvec vec 0 idx) (subvec vec (inc idx))))

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

(defn variable?
  "True if `arg` is a string starting with an upper-case letter."
  [arg]
  (if (string/blank? arg)
      false
      (let [first-char (subs arg 0 1)
            first-char-alpha (apply str (re-seq #"[a-zA-Z]" first-char))]
        (= first-char
           first-char-alpha
           (string/upper-case first-char)))))

(defn ground? [arg]
  (not (variable? arg)))

(defn matches?
  "Literals `a` and `b` match if they have the same predicate, same arity, and
  any ground arguments are the same."
  [a b]
  (and (= (:predicate a)
          (:predicate b))
       (= (count (:args a))
          (count (:args b)))
       (not-any? (fn [[a-arg b-arg]]
                   (and (not= a-arg b-arg)
                        (ground? a-arg)
                        (ground? b-arg)))
                 (map vector (:args a) (:args b)))))

(defn grounds?
  "Literal `b` grounds literal `a` if they have the same predicate, same arity,
  and at least one shared argument is ground in `b` and variable in `a`. "
  [a b]
  (and (= (:predicate a)
          (:predicate b))
       (= (count (:args a))
          (count (:args b)))
       (some (fn [[a-arg b-arg]]
               (and (variable? a-arg)
                    (ground? b-arg)))
             (map vector (:args a) (:args b)))))

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

(defn internal-names [rule]
  (->> rule
       :body
       (mapcat :args)
       distinct ;; internals share a namespace
       (remove #(= % :unspecified))
       (remove ground?)
       (remove (fn [internal]
                 (some #(= internal %) (-> rule :head :args))))
       vec))

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

(defn find-rule-matches
  "Find head literals that match with body literals from other rules."
  [program]
  (->> (:rules program)
       (map
        (fn [[rule-id rule]]
          (->> (:body rule)
               (mapv
                (fn [body-literal-id]
                  (let [body-literal (get (:literals program) body-literal-id)]
                    (->> (:rules program)
                         (map (fn [[id rule]] [id rule]))
                         (filter (fn [[_ grounding-rule]]
                                   (matches? body-literal (get (:literals program)
                                                               (:head grounding-rule)))))
                         (map (fn [[grounding-rule-id grounding-rule]]
                                {:src  grounding-rule-id
                                 :dest [rule-id body-literal-id]})))))))))
       flatten))

(defn remove-literal [program literal-id]
  (-> program
      ;; Remove the literal itself
      (update :literals #(dissoc % literal-id))
      ;; Remove references to the literal in rules
      (update :rules
              (fn [rules]
                (map-vals
                 (fn [rule]
                   (update rule
                           :body
                           (fn [body]
                             (vec (remove #(= literal-id %) body)))))
                 rules)))))

