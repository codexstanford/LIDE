(ns lide.yscript.db)

(def initial-db
  {:program
   {:target :yscript
    :facts {}
    :rules {}}

   :goal-rule
   nil

   :fact-values
   {}

   :positions
   {:fact {}
    :rule {}}})

(def sample-db
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

   :goal-rule
   nil

   :fact-values
   {"water is falling from the sky" {:value false}}

   :positions
   {:rule {"a" {:x 25, :y 40}
           "b" {:x 325, :y 111}
           "c" {:x 625, :y 147}}}})
