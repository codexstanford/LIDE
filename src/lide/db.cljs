(ns lide.db
  (:require
   [lide.util :as util]))

(def default-db
  (let [applies-id (random-uuid)
        applies-rd-type-id (random-uuid)
        applies-rd-iu-type-id (random-uuid)
        applies-rd-iu-location-id (random-uuid)
        rd-type-id (random-uuid)
        rd-iu-type-id (random-uuid)
        rd-iu-location-id (random-uuid)
        rd400-type-id (random-uuid)
        rd400-iu-type-id (random-uuid)
        rd400-iu-location-id (random-uuid)]
    {:program
     {:literals
      (into
       {}
       [[applies-id  {:predicate "applies"
                      :args ["RD" "ComplianceOption"]}]
        [applies-rd-type-id {:predicate "rd_type"
                             :args ["RD" "RDType"]}]
        [applies-rd-iu-type-id {:predicate "rd_iu_type"
                                :args ["RD" "IUType"]}]
        [applies-rd-iu-location-id {:predicate "rd_iu_location"
                                    :args ["RD" "IULocation"]}]
        [(random-uuid) {:predicate "rd_type"
                        :args ["rd400" "for_sale"]}]
        [(random-uuid) {:predicate "rd_iu_type"
                        :args ["rd400" "for_sale"]}]
        [rd400-iu-location-id {:predicate "rd_iu_location"
                               :args ["rd400" "on_site"]}]
        [(random-uuid) {:predicate "rd_iu_share"
                        :args ["rd400" "ami110" "0.15"]}]])

      :rules
      [{:head applies-id
        :body [applies-rd-type-id
               applies-rd-iu-type-id
               applies-rd-iu-location-id]}]}

     :literal-positions
     {rd400-iu-location-id {:x 220 :y 240}}

     :rule-positions
     {0 {:x 10 :y 10}}

     :graph-transform
     (util/dom-matrix-to-vals (js/DOMMatrix.))}))
