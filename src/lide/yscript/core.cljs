(ns lide.yscript.core
  (:require
   [clojure.set :as set]))

(defn facts-mentioned-in-expression
  "Return a set of all facts mentioned in `expr`."
  [expr]
  (if (uuid? expr)
    #{expr} ;; `expr` is just a fact
    (case (:type expr)
      :and (set (mapcat facts-mentioned-in-expression (:exprs expr)))
      :or  (set (mapcat facts-mentioned-in-expression (:exprs expr))))))

(defn facts-mentioned-in-statement
  "Return a set of all facts mentioned in `statement`."
  [statement]
  (case (:type statement)
    :only-if (conj (facts-mentioned-in-expression (:src-expr statement))
                   (:dest-fact statement))))

(defn facts-mentioned-in-rule
  "Return a set of all facts mentioned in `rule`."
  [rule]
  (apply set/union (map facts-mentioned-in-statement (vals (:statements rule)))))

(defn facts-determined-by-statement
  "Return a set of all facts that can be determined by executing `statement`."
  [statement]
  (case (:type statement)
    :only-if #{(:dest-fact statement)}))

(defn facts-determined-in-rule
  "Return a map of statement ID to set of facts that can be determined by
  executing that statement (looks through all statements in `rule`)."
  [rule]
  (->> (:statements rule)
       (map
        (fn [[st-id st]]
          [st-id (facts-determined-by-statement st)]))
       (into {})))

(defn facts-required-by-statement
  "Return a set of all facts required for the execution of `rule`."
  [statement]
  (case (:type statement)
    :only-if (facts-mentioned-in-expression (:src-expr statement))))

(defn facts-required-by-rule
  "Return a set of all facts required for the execution of `rule`."
  [rule]
  (apply set/union (map facts-required-by-statement (vals (:statements rule)))))

(defn orphan-facts
  "Return a set of all facts in `program` not mentioned in any rule in `program`."
  [program]
  (->> (:rules program)
       (reduce
        (fn [non-orphans [_ rule]]
          (set/union non-orphans
                     (facts-mentioned-in-rule rule)))
        #{})
       (set/difference (set (keys (:facts program))))))

(defn rules-by-required-fact
  "Return a map of fact ID to a set of IDs of all rules requiring that fact."
  [program]
  (->> (:rules program)
       (reduce
        (fn [acc [rule-id rule]]
          (->> (facts-required-by-rule rule)
               (map #(vector % #{rule-id}))
               (into {})
               (merge-with set/union acc)))
        {})))

(defn statements-by-determined-fact
  "Return a map from fact ID to set of IDs of statements that can determine a value
  for that fact."
  [program]
  (->> (:rules program)
       (reduce
        (fn [determiners [rule-id rule]]
          (->> (facts-determined-in-rule rule)
               (mapcat
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
                (fn [[st-id _]]
                  [st-id rule-id]))
               (into acc)))
        {})))
