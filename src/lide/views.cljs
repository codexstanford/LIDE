(ns lide.views
  (:require
   [clojure.string :as string]
   [re-frame.core :as re-frame]
   [lide.subs :as subs]
   ))

(def program
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
           :args ["RD" "IULocation"]}}])

(defn literal-to-epilog [literal]
  (str (:predicate literal)
       "("
       (string/join ", " (:args literal))
       ")"))

(defn rule-to-epilog [rule]
  (let [head (literal-to-epilog (:head rule))
        body (when (seq (:body rule))
               (string/join " &\n\t" (map literal-to-epilog (:body rule))))]
    (str head
         (when body
           (str " :-\n\t" body)))))

(defn rules-view-model [program]
  ;; TODO support disjunction - we might already have a definition
  (map
   (fn [rule]
     (let [internals (->> rule
                          :body
                          (mapcat :args)
                          (filter (fn [internal]
                                    (not (some #(= internal %) (-> rule :head :args)))))
                          vec)]
       {:head (:head rule)
        :internals internals}))
   program))

(defn connections-view-model [program]
  (let [rules-by-head-pred (->> program
                                (map #(vector (-> % :head :predicate) %))
                                (into {}))
        connections-nested (map
                            (fn [rule]
                              (map
                               (fn [body-literal]
                                 (map-indexed
                                  (fn [i arg]
                                    [i arg]
                                    {:src  [(:predicate body-literal)
                                            (as-> body-literal x
                                              (:predicate x)
                                              (get rules-by-head-pred x)
                                              (-> x :head :args)
                                              (get x i))]
                                     :dest [(-> rule :head :predicate)
                                            arg]})
                                  (:args body-literal)))
                               (:body rule)))
                            program)]
    (->> connections-nested
         flatten
         (remove nil?))))

(def rule-positions
  {"applies" {:x 0 :y 0}
   "rd_type" {:x 220 :y 0}
   "rd_iu_type" {:x 220 :y 80}
   "rd_iu_location" {:x 220 :y 160}})

(def rule-head-font-size 18)
(def rule-head-padding 6)
(def rule-binding-font-size 16)
(def rule-binding-padding-x 6)
(def rule-binding-padding-y 3)

(defn rule-layout [rule]
  (let [name-height (+ rule-head-padding
                       rule-head-font-size
                       rule-head-padding)
        head {:predicate (-> rule :head :predicate)
              :position {:x rule-head-padding
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
              (-> rule :head :args))
        args-height (* arg-height (-> rule :head :args count))
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
    {:head head
     :args (into {} args)
     :internals (into {} internals)
     :container {:position (get rule-positions (-> rule :head :predicate) {:x 0 :y 0})
                 :size {:width  150
                        :height (+ name-height args-height internals-height)}}}))

(defn rule [{:keys [rule layout]}]
  [:g {:transform (str "translate(" (-> layout :container :position :x) "," (-> layout :container :position :y) ")")
       :key (-> rule :head :predicate)}
   [:rect {:class  "rule__bg"
           :width  (->> layout :container :size :width)
           :height (->> layout :container :size :height)}]
   [:text {:x rule-head-padding
           :y (/ (+ rule-head-padding rule-head-font-size rule-head-padding) 2)}
    (-> rule :head :predicate)]
   [:<>
    (map
     (fn [[arg arg-layout]]
       [:text {:x rule-binding-padding-x
               :y (->> arg-layout :position :y)
               :key arg}
        arg])
     (:args layout))]
   [:<>
    (map
     (fn [[arg arg-layout]]
       [:text {:x rule-binding-padding-x
               :y (->> arg-layout :position :y)
               :key arg}
        arg])
     (:internals layout))]])

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
            :stroke "black"
            :key (str start-rule ":" start-value "->" end-rule ":" end-value)}]))

(defn main-panel []
  (let [rules-vm (rules-view-model program)
        rule-vms-by-head (->> rules-vm
                              (map #(vector (-> % :head :predicate) %))
                              (into {}))
        rule-layouts (->> rule-vms-by-head
                          (map
                           (fn [[head pred]]
                             [head (rule-layout pred)]))
                          (into {}))
        connections-vm (connections-view-model program)]
    [:svg {:height 500
           :width  1000}
     [:rect {:class "graph__bg"
             :height 500
             :width  1000}]
     (map #(rule {:rule  %
                  :layout (get rule-layouts (-> % :head :predicate))})
          rules-vm)
     (map #(connection {:connection %
                        :rule-layouts rule-layouts})
          connections-vm)]))

#_(defn main-panel []
  (let [name (re-frame/subscribe [::subs/name])]
    [:div
     [:h1
      "Hello from " @name]
     ]))
