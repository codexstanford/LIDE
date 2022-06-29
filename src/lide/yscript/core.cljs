(ns lide.yscript.core
  (:require
   [clojure.set :as set]))

(defn facts-in-expression [expr]
  "Return a set of all facts referenced in `expr`."
  (if (uuid? expr)
    #{expr} ;; `expr` is just a fact
    (case (:type expr)
      :and (set (mapcat facts-in-expression (:exprs expr)))
      :or  (set (mapcat facts-in-expression (:exprs expr))))))

(defn facts-in-statement [statement]
  "Return a set of all facts referenced in `statement`."
  (case (:type statement)
    :only-if (conj (facts-in-expression (:src-expr statement))
                   (:dest-fact statement))))

(defn facts-in-rule [rule]
  "Return a set of all facts referenced in `rule`."
  (mapcat facts-in-statement (:statements rule)))

(defn orphan-facts [program]
  "Return a set of all facts in `program` not referenced in any rule in `program`."
  (->> (:rules program)
       (reduce
        (fn [non-orphans [_ rule]]
          (set/union non-orphans
                     (facts-in-rule rule)))
        #{})
       (set/difference (set (keys (:facts program))))))

(defn find-fact-matches [program]
  [])
