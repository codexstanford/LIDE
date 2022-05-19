(ns lide.db
  (:require
   [lide.util :as util]))

(def default-db
  (let [claim-pay-id (random-uuid)
        claim-decline-id (random-uuid)
        plan-in-effect-id (random-uuid)
        partial-day-id (random-uuid)
        exclusion-id (random-uuid)
        exclusion-4-3i-id (random-uuid)
        skydiving-id (random-uuid)
        overlap-id (random-uuid)]
    {:program
     {:literals
      (into
       {}
       [[claim-pay-id  {:predicate "claim.recommendation"
                        :args ["C" "pay"]}]
        [claim-decline-id {:predicate "claim.recommendation"
                           :args ["C" "decline"]}]
        [plan-in-effect-id {:predicate "plan_in_effect"
                            :args ["C"]}]
        [partial-day-id {:predicate "partial_day"
                         :args ["C"]}]
        [exclusion-id {:predicate "exclusion"
                       :args ["C" "E"]}]
        [exclusion-4-3i-id {:predicate "exclusion"
                            :args ["C" "4.3i"]}]
        [skydiving-id {:predicate "claim.activity"
                       :args ["C" "skydiving"]}]])

      :rules
      [{:head claim-pay-id
        :body [plan-in-effect-id
               partial-day-id
               exclusion-id]}
       {:head exclusion-4-3i-id
        :body [skydiving-id]}]}

     :literal-positions
     {}

     :rule-positions
     {0 {:x 10, :y 10}
      1 {:x 453, :y 278}}

     :graph-transform
     (util/dom-matrix-to-vals (js/DOMMatrix.))}))

(defn find-head [db predicate]
  (->> db
       :program
       :rules
       (filter #(= predicate (-> % :head :predicate)))
       first
       :head))
