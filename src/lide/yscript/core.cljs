(ns lide.yscript.core
  (:require
   [clojure.set :as set]
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
  "Return a vector of descriptors of all facts required by `expr`."
  [expr]
  (cond
    (= "fact_expr" (:type expr))
    [(:descriptor expr)]

    (= "not_expr" (:type expr))
    (facts-required-by-expression (:negand expr))

    (contains? #{"and_expr" "or_expr"} (:type expr))
    (concat
     (facts-required-by-expression (:left expr))
     (facts-required-by-expression (:right expr)))))

(defn next-for-goal-fact
  "Find the descriptor of the next fact we need to ask for a value for to
  determine a value for the fact IDed by `goal-descriptor`."
  [program fact-values goal-descriptor]
  ;; TODO cycle detection
  (if (get fact-values goal-descriptor)
    ;; if we already have a value for the goal fact, nothing to do
    nil
    (let [goal-fact (-> program :facts (get goal-descriptor))
          determiners (->> goal-fact :determiners (map :path))]
      (if (not (seq determiners))
        ;; if this is a leaf fact, then need to ask for a value directly
        goal-descriptor
        ;; otherwise, look through determiner statements
        (some
         (fn [[rule-name statement-idx]]
           (some
            #(next-for-goal-fact program fact-values %)
            (facts-required-by-expression (-> program :rules (get rule-name) :statements (nth statement-idx) :src_expr))))
         determiners)))))

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
