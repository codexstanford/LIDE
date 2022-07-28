(ns lide.yscript.db
  (:require
   [clojure.set :as set]
   [lide.util :as util]
   [lide.yscript.core :as ys]))

(def default-db
  {:program
   {:target :yscript

    :facts
    {"it is raining"
     {:determiners [{:path ["c" 0]}]
      :requirers   [{:path ["b" 0 "src_expr"]
                     :source-position [{:row 0,  :column 0}
                                       {:row 50, :column 67}]}]}}

    :rules
    {"a" {:statements
          [{:type "only_if"
            :dest_fact {:text "you should take an umbrella"
                        :startPosition {:row 0, :column 0}}
            :src_expr {:type "and_expr"
                       :left {:type "fact_expr"
                              :descriptor "you have an umbrella"}
                       :right {:type "fact_expr"
                               :descriptor "you might need an umbrella"}}}]}

     "b" {:statements
          [{:type "only_if"
            :dest_fact "you might need an umbrella"
            :src_expr {:type "or_expr"
                       :left {:type "fact_expr"
                              :descriptor "it is raining"}
                       :right {:type "fact_expr"
                               :descriptor "it looks like it might rain"}}}]}

     "c" {:statements
          [{:type "only_if"
            :dest_fact "it is raining"
            :src_expr {:type "fact_expr"
                       :descriptor "water is falling from the sky"}}]}}}

   :fact-values
   {"water is falling from the sky" {:value false}}

   :positions
   {:rule {"a" {:x 25, :y 40}
           "b" {:x 325, :y 111}
           "c" {:x 625, :y 147}}}})

(defn rule-names
  "Return a set of names of all rules in `db`."
  [db]
  (->> (get-in db [:program :rules])
       vals
       (map :name)
       set))

(defn fact-by-descriptor
  "Find a [ID, fact] pair in `db` matching `descriptor`."
  [db descriptor]
  (some (fn [[id fact]]
          (when (= descriptor (:descriptor fact))
            [id fact]))
        (get-in db [:program :facts])))

(defn rule-by-name
  "Find a [ID, rule] pair in `db` matching `name`."
  [db name]
  (some (fn [[id rule]]
          (when (= name (:name rule))
            [id rule]))
        (get-in db [:program :rules])))

(defn ensure-fact
  "Find a fact with `descriptor` in `db`, or create one.

  Returns a vector of the possibly updated DB, the fact ID, and the fact."
  [db descriptor]
  (let [[found-id found-fact] (fact-by-descriptor db descriptor)
        id (or found-id (random-uuid))
        fact (or found-fact (assoc (ys/default-fact) :descriptor descriptor))
        db-with-fact (if found-id
                       db
                       (assoc-in db [:program :facts id] fact))]
    [db-with-fact id fact]))

(defn ingest-expr-type [type]
  (case type
    :or-expr :or
    :and-expr :and))

(defn ingest
  "Update `db` with values obtained from a yscript AST.

  Returns a pair of the updated `db` and maybe some extra information, depending
  on the type of the AST's root node."
  ;; TODO this is not very efficient
  ([ast]
   (ingest {:program {:target :yscript}
            :rule-source-order []}
           []
           ast))
  ([db path [node-type & children]]
   (cond
     (= node-type :fact-expr)
     (let [[[_ & descriptor-tokens]] children
           descriptor (apply str descriptor-tokens)
           [db-with-fact id _] (ensure-fact db descriptor)]
       [db-with-fact id])

     (= node-type :code)
     (reduce
      (fn [[db'] block]
        (ingest db' [:program] block))
      [db]
      children)

     (= node-type :rule)
     (let [[[_ [_ rule-type] [_ & name-tokens]]
            [_ & statements]] children
           name (apply str name-tokens)
           [found-id rule] (rule-by-name db name)
           id (or found-id (random-uuid))
           db-with-rule (if found-id
                          db
                          (-> db
                              (assoc-in [:program :rules id]
                                        (ys/named-rule name))
                              (update :rule-source-order #(conj % id))))]
       (->> statements
            (reduce
             (fn [[[db'] statement-idx] statement]
               [(ingest db' [:program :rules id :statements statement-idx] statement)
                (inc statement-idx)])
             [[db-with-rule] 0])
            first))

     (= node-type :is-assignment)
     (let [[[_ & dest-descriptor-tokens]
            _
            src-expr-parsed] children
           [db' dest-fact-id _] (ensure-fact db (apply str dest-descriptor-tokens))
           [db'' src-expr] (ingest db' path src-expr-parsed)
           id (random-uuid)]
       ;; TODO this is not quite right, don't need new statement every time
       [(-> db''
            (assoc-in [:program :statements id]
                      {:type :only-if
                       :dest-fact dest-fact-id
                       :src-expr src-expr})
            (assoc-in path id))])

     (contains? #{:and-expr :or-expr} node-type)
     (let [[db-with-facts fact-ids]
           (reduce
            (fn [[db' ids] fact-expr]
              (let [[db-with-fact id] (ingest db' path fact-expr)]
                [db-with-fact (conj ids id)]))
            [db []]
            children)]
       [db-with-facts
        {:type (ingest-expr-type node-type)
         :exprs fact-ids}]))))

(defn rekey-facts
  "Update fact references in `expr` according to `fact-key-renames`."
  [expr fact-key-renames]
  (cond
    (uuid? expr)
    (get fact-key-renames expr)

    (contains? #{:and :or} (:type expr))
    (update expr :exprs #(mapv (fn [subexpr]
                                 (rekey-facts subexpr fact-key-renames))
                               %))

    (= :only-if (:type expr))
    (-> expr
        (update :dest-fact #(rekey-facts % fact-key-renames))
        (update :src-expr #(rekey-facts % fact-key-renames)))))

(defn reconcile-ids
  "Replace rule and fact IDs in `into-db` with their correspondents in
  `prior-db`."
  [into-db prior-db]
  (let [rule-key-renames
        (->> (get-in into-db [:program :rules])
             (map (fn [[into-id rule]]
                    (let [[prior-id _] (rule-by-name prior-db (:name rule))]
                      [into-id (or prior-id into-id)])))
             (into {}))

        fact-key-renames
        (->> (get-in into-db [:program :facts])
             (map (fn [[into-id fact]]
                    (let [[prior-id _] (fact-by-descriptor prior-db (:descriptor fact))]
                      [into-id (or prior-id into-id)])))
             (into {}))]
    (-> into-db
        (update-in [:program :rules] #(set/rename-keys % rule-key-renames))
        (update-in [:program :facts] #(set/rename-keys % fact-key-renames))
        (update-in [:program :statements]
                   (fn [statements]
                     (util/map-vals #(rekey-facts % fact-key-renames)
                                    statements))))))

(defn populate-expr [program expr]
  (cond
    (uuid? expr)
    (get-in program [:facts expr])

    (contains? #{:and :or} (:type expr))
    (update expr
            :exprs
            (fn [exprs]
              (mapv #(populate-expr program %) exprs)))))

(defn populate-statement [program statement]
  (case (:type statement)
    :only-if (-> statement
                 (update :dest-fact #(get-in program [:facts %]))
                 (update :src-expr #(populate-expr program %)))))

(defn populate-rule [program rule]
  (update rule
          :statements
          (fn [statements]
            (->> statements
                 (map
                  (fn [st-id]
                    [st-id (populate-statement program (get-in program [:statements st-id]))]))
                 (into {})))))

(defn populate-program
  "Replace IDs throughout `program` with their actual values."
  [program]
  (update program
          :rules
          (fn [rules]
            (util/map-vals
             (fn [rule]
               (populate-rule program rule))
             rules))))
