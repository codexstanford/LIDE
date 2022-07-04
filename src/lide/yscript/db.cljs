(ns lide.yscript.db
  (:require
   [lide.util :as util]))

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
      {rule-1-id {:name ""
                  :goal true
                  :statements [take-if-id]}

       rule-2-id {:name ""
                  :goal false
                  :statements [might-need-if-id]}

       rule-3-id {:name ""
                  :goal false
                  :statements [raining-if-id]}}}

     :positions
     {rule-1-id {:x 25, :y 40}
      rule-2-id {:x 325, :y 111}
      rule-3-id {:x 625, :y 147}}}))

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
