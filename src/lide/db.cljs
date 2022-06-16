(ns lide.db
  (:require
   [lide.util :as util]))

(def default-db
  (let [;; Literal IDs
        claim-pay-id (random-uuid)
        claim-decline-id (random-uuid)
        plan-in-effect-id (random-uuid)
        partial-day-id (random-uuid)
        exclusion-id (random-uuid)
        exclusion-4-3i-id (random-uuid)
        skydiving-id (random-uuid)
        overlap-id (random-uuid)

        ;; Rule IDs
        claim-pay-rule-id (random-uuid)
        plan-in-effect-rule-id (random-uuid)
        skydiving-rule-id (random-uuid)]
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
      (into
       {}
       [[claim-pay-rule-id
         {:head claim-pay-id
          :body [plan-in-effect-id]}]
        [plan-in-effect-rule-id
         {:head plan-in-effect-id
          :body []}]
        [skydiving-rule-id
         {:head exclusion-4-3i-id
          :body [skydiving-id]}]])

      ;; These individual defeater/defeated relationships together define a
      ;; superiority relation in the manner of Nute's Defeasible Logic.
      :defeatings
      #{{:defeater skydiving-rule-id :defeated claim-pay-rule-id}}}

    :collapsed-literals
    {}

    :rule-positions
    (into
     {}
     [[claim-pay-rule-id {:x 10, :y 10}]
      [plan-in-effect-rule-id {:x 453, :y 278}]])

    :graph-transform
    (util/dom-matrix-to-vals (js/DOMMatrix.))}))
