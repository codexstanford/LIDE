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
   :graph-transform (util/dom-matrix-to-vals (js/DOMMatrix.))})
