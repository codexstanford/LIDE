(ns lide.db
  (:require
   [lide.util :as util]))

(def default-db
  {:program
   [{:head {:predicate "applies"
            :args ["RD" "ComplianceOption"]}
     :body [{:predicate "rd_type"
             :args ["RD" "RDType"]}
            {:predicate "rd_iu_type"
             :args ["RD" "IUType"]}
            {:predicate "rd_iu_location"
             :args ["RD" "IULocation"]}]}
    {:head {:predicate "rd_type"
            :args ["RD" "Type"]}}
    {:head {:predicate "rd_iu_type"
            :args ["RD" "IUType"]}}
    {:head {:predicate "rd_iu_location"
            :args ["RD" "IULocation"]}}]
   :rule-positions {"applies" {:x 0 :y 0}
                    "rd_type" {:x 220 :y 0}
                    "rd_iu_type" {:x 220 :y 120}
                    "rd_iu_location" {:x 220 :y 240}}
   :graph-transform (util/dom-matrix-to-vals (js/DOMMatrix.))})
