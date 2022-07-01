(ns lide.yscript.core
  (:require
   [clojure.set :as set]))

(defn facts-required-by-expression
  "Return a set of all facts required by `expr`."
  [expr]
  (if (uuid? expr)
    #{expr} ;; `expr` is just a fact
    (case (:type expr)
      :and (apply set/union (map facts-required-by-expression (:exprs expr)))
      :or  (apply set/union (map facts-required-by-expression (:exprs expr))))))

(defn facts-required-by-statement
  "Return a set of all facts required for the execution of `rule`."
  [statement]
  (case (:type statement)
    :only-if (facts-required-by-expression (:src-expr statement))))

(defn facts-determined-by-statement
  "Return a set of all facts that can be determined by executing `statement`."
  [statement]
  (case (:type statement)
    :only-if #{(:dest-fact statement)}))

(defn statements-by-required-fact
  "Return a map of fact ID to a set of IDs of all statements requiring that fact."
  [program]
  (->> (:statements program)
       (reduce
        (fn [acc [st-id statement]]
          (->> (facts-required-by-statement statement)
               (map #(vector % #{st-id}))
               (into {})
               (merge-with set/union acc)))
        {})))

(defn statements-by-determined-fact
  "Return a map from fact ID to set of IDs of statements that can determine a value
  for that fact."
  [program]
  (->> (:statements program)
       (reduce
        (fn [determiners [st-id statement]]
          (->> (facts-determined-by-statement statement)
               (map #(vector % #{st-id}))
               #_(mapcat
                (fn [[st-id facts]]
                  (map #(vector % #{st-id}) facts)))
               (into {})
               (merge-with set/union determiners)))
        {})))

(defn rules-by-statement
  "Return a map from statement ID to ID of the rule containing that statement."
  [program]
  (->> (:rules program)
       (reduce
        (fn [acc [rule-id rule]]
          (->> (:statements rule)
               (map
                (fn [st-id]
                  [st-id rule-id]))
               (into acc)))
        {})))

(defn compute-expression
  "Compute the value of `expression` given the information in `program`.

  It may not be possible to determine a value, in which case `:unknown` is
  returned.

  TODO this would memoize very nicely"
  [program expr]
  (cond
    (uuid? expr)
    (get-in program [:facts expr :value])

    (= :and (:type expr))
    (cond
      (some #(= false (compute-expression program %)) (:exprs expr)) false
      (every? #(= true (compute-expression program %)) (:exprs expr)) true
      :else :unknown)

    (= :or (:type expr))
    (cond
      (some #(= true (compute-expression program %)) (:exprs expr)) true
      (every? #(= false (compute-expression program %)) (:exprs expr)) false
      :else :unknown)))

(defn execute-statement
  "Determine whatever fact values `statement` can. Return `program` with these
  fact values updated."
  [program statement]
  (case (:type statement)
    :only-if (let [value (compute-expression program (:src-expr statement))]
               (assoc-in program [:facts (:dest-fact statement) :value] value))))

(defn forward-chain
  "Infer as much as possible from fact `fact-id` and return `program` with new
  conclusions added."
  [program index fact-id]
  (->> (get-in index [:statements-by-required-fact fact-id])
       ;; TODO should handle rule order
       (reduce (fn [program' statement-id]
                 (let [statement (get-in program [:statements statement-id])
                       program'' (execute-statement program' statement)]
                   (->> statement
                        facts-determined-by-statement
                        (reduce (fn [program''' determined-fact]
                                  (forward-chain program''' index determined-fact))
                                program''))))
               program)))
