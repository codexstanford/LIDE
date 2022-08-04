(ns lide.yscript.core
  (:require
   [clojure.set :as set]
   [clojure.string :as string]
   [lide.util :as util]))

(defn rule-names
  "Return a collection of all rule names in use in `program`."
  [program]
  (-> program :rules keys))

(defn name-rule
  "Generate a rule name that doesn't already exist in `existing-names`.

  This will be something like 'rule 1', falling back to 'rule 2', ..."
  ([existing-names]
   (name-rule existing-names 1))
  ([existing-names attempt]
   (let [try-name (str "rule " attempt)]
     (if (not (contains? existing-names try-name))
       try-name
       (name-rule existing-names (inc attempt))))))

(defn facts-required-by-expression
  "Return a set of descriptors of all facts required by `expr`."
  [expr]
  (cond
    (= "fact_expr" (:type expr))
    #{(:descriptor expr)}

    (contains? #{"and_expr" "or_expr"} (:type expr))
    (set/union (facts-required-by-expression (:left expr))
               (facts-required-by-expression (:right expr)))))

(defn facts-determined-by-statement
  "Return a set of all facts that can be determined by executing `statement`."
  [statement]
  (case (:type statement)
    "only_if" #{(:dest_fact statement)}))

(defn compute-expression
  "Compute the value of `expression` given the information in `program` and
  `fact-values`.

  It may not be possible to determine a value, in which case `:unknown` is
  returned.

  TODO this would memoize very nicely"
  [program fact-values expr]
  (cond
    (= :unspecified expr)
    :unknown

    (= "fact_expr" (:type expr))
    (get-in fact-values [(:descriptor expr) :value] :unknown)

    (= "and_expr" (:type expr))
    (let [left (compute-expression program fact-values (:left expr))]
      (if (= false left)
        false
        (let [right (compute-expression program fact-values (:right expr))]
          (case [left right]
            ;; We know `left` isn't false, so 2x3 = 6 remaining possibilities
            [:unknown :unknown] :unknown
            [:unknown false] false
            [:unknown true] :unknown
            [true :unknown] :unknown
            [true false] false
            [true true] true))))

    (= "or_expr" (:type expr))
    (let [left (compute-expression program fact-values (:left expr))]
      (if (= true left)
        true
        (let [right (compute-expression program fact-values (:right expr))]
          (case [left right]
            ;; similarly to AND above
            [:unknown :unknown] :unknown
            [:unknown false] :unknown
            [:unknown true] true
            [false :unknown] :unknown
            [false false] false
            [false true] true))))))

(defn compute-statement
  "Determine whatever fact values `statement` can, and return a map of descriptors
  of such facts to their determined values."
  [program fact-values statement]
  (case (:type statement)
    "only_if"
    {(-> statement :dest_fact :descriptor)
     (compute-expression program fact-values (:src_expr statement))}))

(defn forward-chain
  "Infer as much as possible from fact `fact-id` and update `fact-values`
  accordingly."
  [program fact-values fact-id]
  (let [fact (get-in program [:facts fact-id])]
    (->> (:requirers fact)
         (map :path)
         ;; TODO should handle rule order
         (reduce (fn [fact-values' [rule-name statement-idx & exprs]]
                   (let [statement (get-in program [:rules rule-name :statements statement-idx])
                         computed-values (compute-statement program fact-values' statement)]
                     (reduce (fn [fact-values'' [descriptor computed-value]]
                               (forward-chain program
                                              (assoc-in fact-values''
                                                        [descriptor :value]
                                                        computed-value)
                                              descriptor))
                             fact-values'
                             computed-values)))
                 fact-values))))

(defn parse-positions
  "Parse position data as read from .lide/positions.json."
  [^js positions]
  (-> (js->clj positions)
      (clojure.set/rename-keys {"rule" :rule})
      (clojure.set/rename-keys {"fact" :fact})
      (update
       :rule
       (fn [rule-positions]
         (util/map-vals
          (fn [position]
            {:x (get position "x")
             :y (get position "y")})
          rule-positions)))))
