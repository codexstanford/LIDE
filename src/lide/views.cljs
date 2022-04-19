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
  [{:src ["rd_type", "RD"]
    :dest ["applies", "RD"]}
   {:src ["rd_type", "Type"]
    :dest ["applies", "RDType"]}
   {:src ["rd_iu_type", "RD"]
    :dest ["applies", "RD"]}
   {:src ["rd_iu_type", "IUType"]
    :dest ["applies", "IUType"]}
   {:src ["rd_iu_location", "RD"]
    :dest ["applies", "RD"]}
   {:src ["rd_iu_location", "IULocation"]
    :dest ["applies", "IULocation"]}])

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
     :args (into {} args)
     :internals (into {} internals)
     :container {:position (get rule-positions (:name rule) {:x 0 :y 0})
                 :size {:width  150
                        :height (+ name-height args-height internals-height)}}}))

(defn rule [{:keys [rule layout]}]
  [:g {:transform (str "translate(" (-> layout :container :position :x) "," (-> layout :container :position :y) ")")
       :key (:name rule)}
   [:rect {:class  "rule__bg"
           :width  (->> layout :container :size :width)
           :height (->> layout :container :size :height)}]
   [:text {:x rule-name-padding
           :y (/ (+ rule-name-padding rule-name-font-size rule-name-padding) 2)}
    (:name rule)]
   [:<>
    (map
     (fn [[arg arg-layout]]
       [:text {:x rule-binding-padding-x
               :y (->> arg-layout :position :y)}
        arg])
     (:args layout))]])

(defn socket-position [rule-layout value {:keys [end]}]
  (let [all-names (merge (:args rule-layout) (:internals rule-layout))]
    {:x (+ (-> rule-layout :container :position :x)
           (if (= end :dest)
             (-> rule-layout :container :size :width)
             0))
     :y (+ (-> rule-layout :container :position :y)
           (-> all-names (get value) :position :y))}))

(defn connection [{:keys [connection rule-layouts]}]
  (let [[start-rule start-value] (:src connection)
        [end-rule end-value]     (:dest connection)
        start-layout (get rule-layouts start-rule)
        end-layout (get rule-layouts end-rule)
        start (socket-position start-layout start-value {:end :src})
        end (socket-position end-layout end-value {:end :dest})]
    [:line {:x1 (:x start)
            :y1 (:y start)
            :x2 (:x end)
            :y2 (:y end)
            :stroke "black"}]))

(defn main-panel []
  (let [rule-layouts (into {} (map
                               #(vector (:name %) (rule-layout %))
                               rules))]
    [:svg {:height 500
           :width  1000}
     [:rect {:class "graph__bg"
             :height 500
             :width  1000}]
     (map #(rule {:rule  %
                  :layout (get rule-layouts (:name %))})
          rules)
     (map #(connection {:connection %
                        :rule-layouts rule-layouts})
          connections)]))

#_(defn main-panel []
  (let [name (re-frame/subscribe [::subs/name])]
    [:div
     [:h1
      "Hello from " @name]
     ]))
