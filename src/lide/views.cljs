(ns lide.views
  (:require
   [clojure.string :as string]
   [re-frame.core :as re-frame]
   [lide.epilog :as epilog]
   [lide.events :as events]
   [lide.subs :as subs]
   [lide.util :as util]
   ))

(defn center-position [{:keys [position size]}]
  {:x (+ (:x position) (/ (:width size) 2))
   :y (+ (:y position) (/ (:height size) 2))})

(defn rules-view-model [program highlighted-connection]
  ;; TODO support disjunction - we might already have a definition
  (map
   (fn [rule]
     (let [internals (->> rule
                          :body
                          (mapcat :args)
                          (remove #(= % :unspecified))
                          (remove (fn [internal]
                                    (some #(= internal %) (-> rule :head :args))))
                          vec)]
       {:head (:head rule)
        :internals internals
        :highlight (filterv (fn [arg]
                              (some #(= % [(-> rule :head :predicate) arg])
                                    (vals (select-keys highlighted-connection [:src :dest]))))
                            (concat (-> rule :head :args) internals))
       }))
   program))

(defn connections-view-model [program highlighted-connection]
  (let [rules-by-head-pred (->> program
                                (map #(vector (-> % :head :predicate) %))
                                (into {}))
        connections-nested (map
                            (fn [rule]
                              (map
                               (fn [body-literal]
                                 (map-indexed
                                  (fn [i arg]
                                    (when (not= arg :unspecified)
                                      (let [src-rule (:predicate body-literal)
                                            src-arg  (as-> body-literal x
                                                       (:predicate x)
                                                       (get rules-by-head-pred x)
                                                       (-> x :head :args)
                                                       (get x i))
                                            dest-rule (-> rule :head :predicate)
                                            dest-arg  arg
                                            unhighlighted {:src  [src-rule src-arg]
                                                           :dest [dest-rule dest-arg]}]
                                        (assoc unhighlighted
                                               :highlighted
                                               (= unhighlighted highlighted-connection)))))
                                  (:args body-literal)))
                               (:body rule)))
                            program)]
    (->> connections-nested
         flatten
         (remove nil?))))

(def rule-head-font-size 18)
(def rule-head-padding 6)
(def rule-binding-font-size 16)
(def rule-binding-padding-x 6)
(def rule-binding-padding-y 3)

(defn rule-layout [rule positions]
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
        internals-height (* arg-height (->> rule :internals count))

        add-binding {:position {:y (+ name-height
                                      args-height
                                      internals-height
                                      (/ arg-height 2))}}]
    {:head head
     :args (into {} args)
     :internals (into {} internals)
     :add-binding add-binding
     :container {:position (get positions (-> rule :head :predicate) {:x 0 :y 0})
                 :size {:width  150
                        :height (+ name-height
                                   args-height
                                   internals-height
                                   ;; Add Binding is one arg tall
                                   arg-height)}}}))

(defn rule [{:keys [rule layout]}]
  [:g {:on-mouse-down #(re-frame/dispatch [::events/start-drag-rule % rule])
       :transform (str "translate(" (-> layout :container :position :x) "," (-> layout :container :position :y) ")")
       :key (-> rule :head :predicate)}
   [:rect {:class  "rule__bg"
           :width  (->> layout :container :size :width)
           :height (->> layout :container :size :height)}]
   [:text {:x rule-head-padding
           :y (/ (+ rule-head-padding rule-head-font-size rule-head-padding) 2)}
    (-> rule :head :predicate)]
   [:<>
    (map-indexed
     (fn [idx [arg arg-layout]]
       [:text {:x rule-binding-padding-x
               :y (->> arg-layout :position :y)
               :class (when (some #(= % arg) (:highlight rule)) "rule--highlight")
               :key arg
               :on-click #(re-frame/dispatch [::events/connect-src (-> rule :head :predicate) [idx arg]])}
        arg])
     (concat (:args layout) (:internals layout)))]
   [:text {:x rule-binding-padding-x
           :y (->> layout :add-binding :position :y)
           :on-click #(re-frame/dispatch [::events/start-connect-dest (-> rule :head :predicate)])}
    "+ Add Binding"]])

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
        end-layout   (get rule-layouts end-rule)
        start (socket-position start-layout start-value {:end :src})
        end   (socket-position end-layout end-value {:end :dest})]
    [:<> {:key (str start-rule ":" start-value "->" end-rule ":" end-value)}
     [:line {:x1 (:x start)
             :y1 (:y start)
             :x2 (:x end)
             :y2 (:y end)
             :stroke (if (:highlighted connection) "green" "black")}]
     [:line {:x1 (:x start)
             :y1 (:y start)
             :x2 (:x end)
             :y2 (:y end)
             :stroke "transparent"
             :stroke-width 10
             :on-mouse-over #(re-frame/dispatch [::events/highlight-connection (select-keys connection [:src :dest])])
             :on-mouse-leave #(re-frame/dispatch [::events/stop-connection-highlight])
             :on-click #(re-frame/dispatch [::events/disconnect connection])}]]))

(defn main-panel []
  (let [program (re-frame/subscribe [::subs/program])
        rule-positions (re-frame/subscribe [::subs/rule-positions])
        connecting-dest (re-frame/subscribe [::subs/connecting-dest])
        mouse-position (re-frame/subscribe [::subs/mouse-position])
        highlighted-connection (re-frame/subscribe [::subs/highlighted-connection])
        graph-transform (re-frame/subscribe [::subs/graph-transform])
        rules-vm (rules-view-model @program @highlighted-connection)
        rule-vms-by-head (->> rules-vm
                              (map #(vector (-> % :head :predicate) %))
                              (into {}))
        rule-layouts (->> rule-vms-by-head
                          (map
                           (fn [[head pred]]
                             [head (rule-layout pred @rule-positions)]))
                          (into {}))
        connections-vm (connections-view-model @program @highlighted-connection)]
    [:div {:id "app-container"
           :on-mouse-up #(re-frame/dispatch [::events/mouse-up %])}
     [:svg {:class "graph-panel"
            :height 500
            :width  1000
            :on-mouse-move (goog.functions.throttle #(re-frame/dispatch [::events/mouse-move %])
                                                    25)
            :on-wheel (goog.functions.throttle #(re-frame/dispatch [::events/scroll-graph %])
                                               100)}
      [:rect {:class "graph__bg"
              :height 500
              :width  1000
              :on-mouse-down #(re-frame/dispatch [::events/start-drag-graph %])}]
      [:g {:class "graph__viewport"
           :transform (when @graph-transform
                        (str (util/dom-matrix-from-vals @graph-transform)))}
       (when (and (not (string/blank? @connecting-dest))
                  @mouse-position)
         (let [origin-center (center-position (get rule-layouts @connecting-dest))]
           [:line {:x1 (:x origin-center)
                   :y1 (:y origin-center)
                   :x2 (:x @mouse-position)
                   :y2 (:y @mouse-position)
                   :stroke "#333"}]))
       (map #(rule {:rule %
                    :layout (get rule-layouts (-> % :head :predicate))})
            rules-vm)
       (map #(connection {:connection %
                          :rule-layouts rule-layouts})
            connections-vm)]]
     [:div {:class "code-panel"}
      [:pre {:class "code"}
       (string/join "\n\n"
                    (map epilog/rule-to-epilog @program))]]]))
