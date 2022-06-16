(ns lide.db
  (:require
   [lide.util :as util]))

(def default-db
  (let [;; Literal IDs
        claim-pay-id (random-uuid)
        claim-decline-id (random-uuid)
        plan-in-effect-id (random-uuid)
        plan-in-effect-head-id (random-uuid)
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
                        :args ["Claim" "pay"]}]
        [claim-decline-id {:predicate "claim.recommendation"
                           :args ["Claim" "decline"]}]
        [plan-in-effect-id {:predicate "plan_in_effect"
                            :args ["Claim"]}]
        [plan-in-effect-head-id {:predicate "plan_in_effect"
                                 :args ["Claim"]}]
        [partial-day-id {:predicate "partial_day"
                         :args ["Claim"]}]
        [exclusion-id {:predicate "exclusion"
                       :args ["Claim" "E"]}]
        [exclusion-4-3i-id {:predicate "exclusion"
                            :args ["Claim" "4.3i"]}]
        [skydiving-id {:predicate "claim.activity"
                       :args ["Claim" "skydiving"]}]])

      :rules
      (into
       {}
       [[claim-pay-rule-id
         {:head claim-pay-id
          :body [plan-in-effect-id]}]
        [plan-in-effect-rule-id
         {:head plan-in-effect-head-id
          :body []}]
        [skydiving-rule-id
         {:head exclusion-4-3i-id
          :body [skydiving-id]}]])

      ;; These individual defeater/defeated relationships together define a
      ;; superiority relation in the manner of Nute's Defeasible Logic (NDL).
      ;; NDL also maintains a distinction between strict and defeasible rules,
      ;; but here I'm just treating all rules as defeasible. I don't mean to
      ;; commit completely to these semantics, and particularly I may compile
      ;; certain things to strict rules at some point, as an implementation
      ;; detail. It does seem... fine... to always treat contract provisions as
      ;; defeasible, but I want to think about it a bit more.
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
