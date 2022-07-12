(ns lide.yscript.db
  (:require
   [lide.util :as util]
   [lide.yscript.core :as ys]))

(def default-db
  (let [take-umbrella-id (random-uuid)
        have-umbrella-id (random-uuid)
        might-need-umbrella-id (random-uuid)
        is-raining-id (random-uuid)
        might-rain-id (random-uuid)
        water-falling-id (random-uuid)

        take-if-id (random-uuid)
        might-need-if-id (random-uuid)
        raining-if-id (random-uuid)

        rule-1-id (random-uuid)
        rule-2-id (random-uuid)
        rule-3-id (random-uuid)]
    {:program
     {:target :yscript

      :facts {take-umbrella-id
              {:type :boolean
               :descriptor "you should take an umbrella"
               :value :unknown}

              have-umbrella-id
              {:type :boolean
               :descriptor "you have an umbrella"
               :value :unknown}

              might-need-umbrella-id
              {:type :boolean
               :descriptor "you might need an umbrella"
               :value :unknown}

              is-raining-id
              {:type :boolean
               :descriptor "it is raining"
               :value :unknown}

              might-rain-id
              {:type :boolean
               :descriptor "it looks like it might rain"
               :value :unknown}

              water-falling-id
              {:type :boolean
               :descriptor "water is falling from the sky"
               :value :unknown}}

      :statements
      {take-if-id
       {:type :only-if
        :dest-fact take-umbrella-id
        :src-expr {:type :and
                   :exprs [have-umbrella-id
                           might-need-umbrella-id]}}
       might-need-if-id
       {:type :only-if
        :dest-fact might-need-umbrella-id
        :src-expr {:type :or
                   :exprs [is-raining-id
                           might-rain-id]}}
       raining-if-id
       {:type :only-if
        :dest-fact is-raining-id
        :src-expr water-falling-id}}

      :rules
      {rule-1-id {:name "a"
                  :goal true
                  :statements [take-if-id]}

       rule-2-id {:name "b"
                  :goal false
                  :statements [might-need-if-id]}

       rule-3-id {:name "c"
                  :goal false
                  :statements [raining-if-id]}}}

     :rule-source-order
     [rule-1-id
      rule-2-id
      rule-3-id]

     :positions
     {rule-1-id {:x 25, :y 40}
      rule-2-id {:x 325, :y 111}
      rule-3-id {:x 625, :y 147}}}))

(defn fact-by-descriptor
  "Find a [ID, fact] pair in `db` matching `descriptor`."
  [db descriptor]
  (some (fn [[id fact]]
          (= descriptor (:descriptor fact)))
        (get-in db [:program :facts])))

(defn rule-by-name [db name]
  (some #(= name (:name %))
        (get-in db [:program :rules])))

(defn ensure-fact
  "Find a fact with `descriptor` in `db`, or create one.

  Returns a vector of the possibly updated DB, the fact ID, and the fact."
  [db descriptor]
  (let [[found-id fact] (fact-by-descriptor db descriptor)
        id (or found-id (random-uuid))
        db-with-fact (if found-id
                       db
                       (assoc-in db
                                 [:program :facts id]
                                 (assoc (ys/default-fact) :descriptor descriptor)))]
    [db-with-fact id fact]))

(defn ingest-expr-type [type]
  (case type
    :or-expr :or
    :and-expr :and))

(defn ingest
  "Update `db` with values obtained from yscript AST `parsed`.

  Returns a pair of the updated `db` and maybe some extra information, depending
  on the type of `parsed`'s root node."
  ;; TODO this is not very efficient
  [db path [node-type & children]]
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
    (let [[[_ [_ rule-type] [_ name]]
           [_ & statements]] children
          [found-id rule] (rule-by-name db name)
          id (or found-id (random-uuid))
          db-with-rule (if found-id
                         db
                         (assoc-in db
                                   [:program :rules id]
                                   (assoc (ys/default-rule) :name name)))]
      (->> statements
           (reduce
            (fn [[[db'] statement-idx] statement]
              [(ingest db' (conj path :rules id :statements statement-idx) statement)
               (inc statement-idx)])
            [[db-with-rule] 0])
           first))

    (= node-type :is-assignment)
    (let [[[_ & dest-descriptor-tokens]
           _
           src-expr-parsed] children
          [db' dest-fact-id _] (ensure-fact db (apply str dest-descriptor-tokens))
          [db'' src-expr] (ingest db' path src-expr-parsed)]
      [(assoc-in db''
                  path
                  {:type :only-if
                   :dest-fact dest-fact-id
                   :src-expr src-expr})])

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
        :exprs fact-ids}])))

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
