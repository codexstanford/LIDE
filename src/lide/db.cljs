(ns lide.db
  (:require
   [lide.util :as util]))

(def default-db
  (let [applies-id (random-uuid)
        rd-type-id (random-uuid)
        rd-iu-type-id (random-uuid)
        rd400-iu-location-id (random-uuid)]
    {:program
     (into {}
           [[applies-id {:head {:predicate "applies"
                                :args ["RD" "ComplianceOption"]}
                         :body [{:predicate "rd_type"
                                 :args ["RD" "RDType"]}
                                {:predicate "rd_iu_type"
                                 :args ["RD" "IUType"]}
                                {:predicate "rd_iu_location"
                                 :args ["RD" "IULocation"]}]}]

             ;; rd400 example

            [(random-uuid) {:head {:predicate "rd_type"
                                   :args ["rd400" "for_sale"]}}]
            [(random-uuid) {:head {:predicate "rd_iu_type"
                                   :args ["rd400" "for_sale"]}}]
            [rd400-iu-location-id {:head {:predicate "rd_iu_location"
                                          :args ["rd400" "on_site"]}}]
            [(random-uuid) {:head {:predicate "rd_iu_share"
                                   :args ["rd400" "ami110" "0.15"]}}]])

     :rule-positions
     {applies-id {:x 0 :y 0}
      rd400-iu-location-id {:x 220 :y 240}}

     :graph-transform
     (util/dom-matrix-to-vals (js/DOMMatrix.))}))
