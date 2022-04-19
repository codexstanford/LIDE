(ns lide.views
  (:require
   [re-frame.core :as re-frame]
   [lide.subs :as subs]
   ))

(def rules
  [{:name "applies"
    :args ["RD" "ComplianceOption"]
    :internals ["RDType" "IUType" "IULocation"]}
   {:name "rd_type"
    :args ["RD" "Type"]}
   {:name "rd_iu_type"
    :args ["RD" "IUType"]}
   {:name "rd_iu_location"
    :args ["RD" "IULocation"]}])

(def connections
  {"applies" [{:rule "rd_type"
               :connections [["RD", "RD"], ["RDType", "Type"]]}
              {:rule "rd_iu_type"
               :connections [["RD", "RD"], ["IUType", "IUType"]]}
              {:rule "rd_iu_location"
               :connections [["RD", "RD"], ["IULocation", "IULocation"]]}
              {:rule "compliance_requires"
               :connections [["RD", "RD"], ["ComplianceOption", "Rule"]]}]})

(def rule-positions
  {"applies" {:x 0 :y 0}
   "rd_type" {:x 220 :y 0}
   "rd_iu_type" {:x 220 :y 80}
   "rd_iu_location" {:x 220 :y 160}})

(def rule-name-font-size 18)
(def rule-name-padding 6)
(def rule-binding-font-size 16)
(def rule-binding-padding-x 6)
(def rule-binding-padding-y 3)

(defn rule-layout [rule]
  (let [name-height (+ rule-name-padding
                       rule-name-font-size
                       rule-name-padding)
        name {:position {:x rule-name-padding
                         :y (/ name-height 2)}
              :size     {:width  150
                         :height name-height}}
        arg-height (+  rule-binding-padding-y
                       rule-binding-font-size
                       rule-binding-padding-y)
        args-y-start (+ name-height
                        (/ arg-height 2))
        args (map-indexed
              (fn [i arg]
                [arg
                 {:position {:y (+ args-y-start
                                   (* i arg-height))}}])
              (:args rule))
        args-height (* arg-height (->> rule :args count))
        internals-y-start (+ name-height
                             args-height
                             (/ arg-height 2))
        internals (map-indexed
                   (fn [i internal]
                     [internal
                      {:position {:y (+ internals-y-start
                                        (* i arg-height))}}])
                   (:internals rule))
        internals-height (* arg-height (->> rule :internals count))]
    {:name name
     :args args
     :internals internals
     :container {:size {:width  150
                        :height (+ name-height args-height internals-height)}}}))

(defn rule [props]
  (let [position (get rule-positions (:name props) {:x 0 :y 0})
        layout (rule-layout props)]
    [:g {:transform (str "translate(" (:x position) "," (:y position) ")")
         :key (:name props)}
     [:rect {:class  "rule__bg"
             :width  (->> layout :container :size :width)
             :height (->> layout :container :size :height)}]
     [:text {:x rule-name-padding
             :y (/ (+ rule-name-padding rule-name-font-size rule-name-padding) 2)}
      (:name props)]
     [:<>
      (map
       (fn [[arg arg-layout]]
         [:text {:x rule-binding-padding-x
                 :y (->> arg-layout :position :y)}
          arg])
       (:args layout))]]))

(defn main-panel []
  [:svg {:height 500
         :width  1000}
   [:rect {:class "graph__bg"
           :height 500
           :width  1000}]
   (map rule rules)])

#_(defn main-panel []
  (let [name (re-frame/subscribe [::subs/name])]
    [:div
     [:h1
      "Hello from " @name]
     ]))
