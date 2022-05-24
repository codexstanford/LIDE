(ns lide.views
  (:require
   [clojure.string :as string]
   [re-frame.core :as rf]
   [lide.epilog :as epilog]
   [lide.events :as events]
   [lide.subs :as subs]
   [lide.util :as util]
   ))

(defn center-position [{:keys [position size]}]
  {:x (+ (:x position) (/ (:width size) 2))
   :y (+ (:y position) (/ (:height size) 2))})

(defn localize-event-to-svg [^js svg event]
  (let [dom-point (js/DOMPoint. (.-clientX event) (.-clientY event))
        svg-point (.matrixTransform dom-point (.inverse (.getScreenCTM svg)))]
    {:x (.-x svg-point)
     :y (.-y svg-point)}))

(defn body-literal-view-model [literal]
  {:predicate (str (when (:negative literal) "~")
                   (:predicate literal))
   :args      (:args literal)})

(defn rule-view-model [highlighted-connection rule-index rule]
  (let [internals (util/internal-names rule)]
    {:head (:head rule)
     :body (mapv body-literal-view-model (:body rule))
     :internals internals
     :highlight (filterv (fn [arg]
                           (some #(= % [rule-index arg])
                                 (vals (select-keys highlighted-connection [:src :dest]))))
                         (concat (-> rule :head :args) internals))}))

(defn rules-view-model [highlighted-connection program]
  ;; TODO support disjunction - we might already have a definition
  (->> (:rules program)
       (map #(util/populate-rule program %))
       (map-indexed (fn [idx rule]
                      (rule-view-model highlighted-connection idx rule)))
       vec))

(defn literal-view-model [highlighted-connection literal-id literal]
  {:predicate (str (when (:negative literal) "~")
                   (:predicate literal))
   :args      (:args literal)
   :highlight (filterv (fn [arg]
                         (some #(= % [literal-id arg])
                               (vals (select-keys highlighted-connection [:src :dest]))))
                       (:args literal))})

(defn filter-by-head [head-literal program]
  (->> program
       (filter (fn [[id rule]]
                 (= (-> rule :head :predicate)
                    (-> head-literal :predicate))))
       ;; TODO filter by args as well?
       (into {})))

(def rule-head-font-size 18)
(def rule-head-padding 6)
(def rule-head-height (+ rule-head-font-size
                         (* 2 rule-head-padding)))
(def rule-binding-font-size 16)
(def rule-binding-padding-x 6)
(def rule-binding-padding-y 3)

(defn literal-layout [literal position]
  (let [name-height (+ rule-head-padding
                       rule-head-font-size
                       rule-head-padding)

        predicate {:predicate (:predicate literal)
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
              (:args literal))
        args-height (* arg-height (-> literal :args count))

        add-argument {:position {:y (+ name-height
                                       args-height
                                       (/ arg-height 2))}}]
    {:predicate predicate
     :args (into {} args)
     :add-argument add-argument
     :container {:position (or position {:x 0 :y 0})
                 :size {:width  150
                        :height (+ name-height
                                   args-height
                                   arg-height)}}}))

(defn literal-collapse [id layout]
  [:<>
   [:rect {:class "rule__button-bg"
           :x (- (-> layout :container :size :width) 40)
           :y 0
           :height rule-head-height
           :width 20
           :on-click #(rf/dispatch [::events/negate-literal id])}
    [:title "Collapse literal"]]
   [:text {:class "rule__button-label"
           :x (- (-> layout :container :size :width) 15)
           :y (/ rule-head-height 2)}
    "-"]])

(defn literal-negate [id layout]
  [:<>
   [:rect {:class "rule__button-bg"
           :x (- (-> layout :container :size :width) 20)
           :y 0
           :height rule-head-height
           :width 20
           :on-click #(rf/dispatch [::events/negate-literal id])}
    [:title "Negate literal"]]
   [:text {:class "rule__button-label"
           :x (- (-> layout :container :size :width) 15)
           :y (/ rule-head-height 2)}
    "~"]])

#_(defn literal [{:keys [id local-position]}]
  (let [literal-raw @(rf/subscribe [::subs/literal id])
        highlighted-connection @(rf/subscribe [::subs/highlighted-connection])
        literal-model (literal-view-model highlighted-connection id literal-raw)
        position @(rf/subscribe [::subs/literal-position id])
        layout (literal-layout literal-model position)]
    [:g {:on-mouse-down #(rf/dispatch [::events/start-drag-literal (local-position %) id])
         :on-click #(rf/dispatch [::events/select-literal id])
         :transform (str "translate(" (-> layout :container :position :x) "," (-> layout :container :position :y) ")")
         :key id}
     [:rect {:class  "rule__bg"
             :width  (->> layout :container :size :width)
             :height (->> layout :container :size :height)}]
     [util/eip-svg-text
      {:value (:predicate literal-model)
       :on-blur #(rf/dispatch [::events/edit-literal-predicate id (-> % .-target .-value)])
       :x rule-head-padding
       :y (/ (+ rule-head-padding rule-head-font-size rule-head-padding) 2)
       :width (->> layout :container :size :width)
       :height (+ rule-head-padding rule-head-font-size rule-head-padding)}]
     [literal-negate id layout]
     [:<>
      (map-indexed
       (fn [arg-index [arg arg-layout]]
         [util/eip-svg-text
          {:value arg
           :on-blur #(rf/dispatch [::events/edit-literal-arg id arg-index (-> % .-target .-value)])
           :x rule-binding-padding-x
           :y (->> arg-layout :position :y)
           :width  (->> layout :container :size :width)
           :height (+ rule-head-padding rule-head-font-size rule-head-padding)
           :display-class (when (some #(= % arg) (:highlight literal-model)) "rule--highlight")
           :key arg-index}])
       (:args layout))]
     [:text {:class "rule__add-arg"
             :x rule-binding-padding-x
             :y (->> layout :add-argument :position :y)
             :on-click #(rf/dispatch [::events/add-literal-argument id])}
      "+ Add argument"]
     [:rect {:class  "rule__border"
             :width  (->> layout :container :size :width)
             :height (->> layout :container :size :height)}]]))

(defn body-literal [{:keys [layout]}]
  (let [id (:id layout)
        highlighted-connection @(rf/subscribe [::subs/highlighted-connection])]
    [:g {:transform (str "translate(" (-> layout :container :position :x) "," (-> layout :container :position :y) ")")}
     [:rect {:class  "rule__bg"
             :width  (->> layout :container :size :width)
             :height (->> layout :container :size :height)}]
     [util/eip-svg-text
      {:value (-> layout :predicate :predicate)
       :on-blur #(rf/dispatch [::events/edit-literal-predicate id (-> % .-target .-value)])
       :x rule-head-padding
       :y (/ (+ rule-head-padding rule-head-font-size rule-head-padding) 2)
       :width (->> layout :container :size :width)
       :height (+ rule-head-padding rule-head-font-size rule-head-padding)}]
     [literal-negate id layout]
     [:<>
      (map-indexed
       (fn [arg-index [arg arg-layout]]
         [util/eip-svg-text
          {:value arg
           :on-blur #(rf/dispatch [::events/edit-literal-arg id arg-index (-> % .-target .-value)])
           :x rule-binding-padding-x
           :y (->> arg-layout :position :y)
           :width  (->> layout :container :size :width)
           :height (+ rule-head-padding rule-head-font-size rule-head-padding)
           :display-style (when (util/variable? arg) {"fill" (util/hash-to-hsl arg)})
           :key arg-index}])
       (:args layout))]
     [:text {:class "rule__add-arg"
             :x rule-binding-padding-x
             :y (->> layout :add-argument :position :y)
             :on-click #(rf/dispatch [::events/add-literal-argument id])}
      "+ Add argument"]
     [:rect {:class  "rule__border"
             :width  (->> layout :container :size :width)
             :height (->> layout :container :size :height)}]]))

(defn rule [{:keys [index local-position]}]
  ;; TODO Should get layout data for EIPs from layout object
  (let [rule-raw @(rf/subscribe [::subs/populated-rule index])
        highlighted-connection @(rf/subscribe [::subs/highlighted-connection])
        rule-model (rule-view-model highlighted-connection index rule-raw)
        ;; layout (rule-layout rule-model position)
        layout @(rf/subscribe [::subs/rule-layout index])
        position @(rf/subscribe [::subs/rule-position index])]
    [:g {:on-mouse-down #(rf/dispatch [::events/start-drag-rule (local-position %) index])
         :on-click #(rf/dispatch [::events/select-rule index])
         :transform (str "translate(" (:x position) "," (:y position) ")")
         :key index}
     [:rect {:class  "rule__bg"
             :width  (->> layout :container :size :width)
             :height (->> layout :container :size :height)}]
     [util/eip-svg-text
      {:value (-> rule-model :head :predicate)
       :on-blur #(rf/dispatch [::events/edit-head-predicate index (-> % .-target .-value)])
       :x rule-head-padding
       :y (/ (+ rule-head-padding rule-head-font-size rule-head-padding) 2)
       :width  (->> layout :container :size :width)
       :height (+ rule-head-padding rule-head-font-size rule-head-padding)}]
     [:<>
      (map-indexed
       (fn [arg-index [arg arg-layout]]
         [util/eip-svg-text
          {:value arg
           :on-blur #(rf/dispatch [::events/edit-head-arg index arg-index (-> % .-target .-value)])
           :x rule-binding-padding-x
           :y (->> arg-layout :position :y)
           :width  (->> layout :container :size :width)
           :height (+ rule-head-padding rule-head-font-size rule-head-padding)
           :display-style (when (util/variable? arg) {"fill" (util/hash-to-hsl arg)})
           :key arg-index}])
       (:args layout))]
     [:<>
      (map-indexed
       (fn [arg-index [arg arg-layout]]
         [:<> {:key arg-index}
          [:rect
           {:class "rule__internal-bg"
            :x 0
            :y (- (-> arg-layout :position :y)
                  (/ (-> arg-layout :size :height) 2))
            :width (-> layout :container :size :width)
            :height (-> arg-layout :size :height) }]
          [:text
           {:x rule-binding-padding-x
            :y (->> arg-layout :position :y)
            :class (when (some #(= % arg) (:highlight rule-model)) "rule--highlight")}
           arg]])
       (:internals layout))]
     [:text {:class "rule__add-arg"
             :x rule-binding-padding-x
             :y (->> layout :add-argument :position :y)
             :on-click #(rf/dispatch [::events/add-argument index])}
      "+ Add argument"]
     [:<>
      (map
       (fn [[id literal-layout]]
         [body-literal
          {:layout literal-layout
           :key id}])
       (:body layout))]
     [:text {:class "rule__add-arg"
             :x rule-binding-padding-x
             :y (->> layout :add-body-literal :position :y)
             :on-click #(rf/dispatch [::events/add-body-literal index])}
      "+ Add subgoal"]
     [:rect {:class  "rule__border"
             :width  (->> layout :container :size :width)
             :height (->> layout :container :size :height)}]]))

(defn socket-position [rule-layout literal-id {:keys [end]}]
  {:x (+ (-> rule-layout :container :position :x)
         (cond
           (and (= end :dest) (not= literal-id :unbound))
           (- (-> rule-layout :container :size :width) 5)

           (and (= end :dest) (= literal-id :unbound))
           (-> rule-layout :container :size :width)

           (and (= end :start) (not= literal-id :unbound))
           5

           :else
           0))
   :y (+ (-> rule-layout :container :position :y)
         (/ rule-head-height 2)
         (if (= literal-id :unbound)
           0
           (get-in rule-layout [:body literal-id :container :position :y])))})

#_(defn composition-connector [connection]
  (let [[start-rule-idx start-arg] (:src connection)
        [end-literal-id end-arg]   (:dest connection)
        start-rule @(rf/subscribe [::subs/populated-rule start-rule-idx])
        start-rule-position @(rf/subscribe [::subs/rule-position start-rule-idx])
        end-literal @(rf/subscribe [::subs/literal end-literal-id])
        end-literal-position @(rf/subscribe [::subs/literal-position end-literal-id])
        highlighted-connection @(rf/subscribe [::subs/highlighted-connection])
        start-layout (rule-layout (rule-view-model highlighted-connection start-rule-idx start-rule)
                                  start-rule-position)
        end-layout   (literal-layout (literal-view-model highlighted-connection
                                                         end-literal-id
                                                         end-literal)
                                     end-literal-position)
        start (socket-position start-layout start-arg {:end :dest})
        end   (socket-position end-layout end-arg {:end :src})]
    [:<>
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
             :on-mouse-over #(rf/dispatch [::events/highlight-connection (select-keys connection [:src :dest])])
             :on-mouse-leave #(rf/dispatch [::events/stop-connection-highlight])}]]))

#_(defn composition-connectors [{:keys [rule-index]}]
  (let [program @(rf/subscribe [::subs/program])
        rule @(rf/subscribe [::subs/rule rule-index])
        highlighted-conn @(rf/subscribe [::subs/highlighted-connection])
        compositions-model (compositions-view-model program rule-index rule highlighted-conn)]
    (into [:<>] (mapv (fn [cm]
                        [composition-connector cm])
                      compositions-model))))

(defn match-connector [{:keys [connection]}]
  (let [[end-rule-idx end-literal-id] (:dest connection)
        start-layout @(rf/subscribe [::subs/rule-layout (:src connection)])
        end-layout   @(rf/subscribe [::subs/rule-layout end-rule-idx])

        draw-connector
        (fn [start end]
          [:<>
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
                   :on-mouse-over #(rf/dispatch [::events/highlight-connection (select-keys connection [:src :dest])])
                   :on-mouse-leave #(rf/dispatch [::events/stop-connection-highlight])
                   :on-click #(rf/dispatch [::events/disconnect connection])}]])]
    (draw-connector (socket-position start-layout :unbound {:end :src})
                    (socket-position end-layout end-literal-id {:end :dest}))))

(defn graph-viewport [{:keys [set-ref]} & children]
  (let [graph-transform @(rf/subscribe [::subs/graph-transform])]
    [:g {:ref set-ref
         :class "graph__viewport"
         :transform (when graph-transform
                      (str (util/dom-matrix-from-vals graph-transform)))}
     children]))

(defn program-graph []
  (let [!svg-viewport (atom nil)]
    (fn []
      (let [local-position (fn [event]
                             (localize-event-to-svg @!svg-viewport event))

            program (rf/subscribe [::subs/program])
            rule-positions (rf/subscribe [::subs/rule-positions])
            literal-positions (rf/subscribe [::subs/literal-positions])
            highlighted-connection (rf/subscribe [::subs/highlighted-connection])
            rules-vm (rules-view-model @highlighted-connection @program)
            #_rule-layouts #_(->> rules-vm
                              (map-indexed
                               (fn [idx rule]
                                 [idx (rule-layout rule (get @rule-positions idx))]))
                              (into {}))
            matches @(rf/subscribe [::subs/matches])]
        [:svg {:class "graph-panel"
               :id "graph-svg"
               :on-mouse-move (goog.functions.throttle #(rf/dispatch [::events/mouse-move (local-position %)])
                                                       25)
               :on-mouse-up #(rf/dispatch [::events/mouse-up (local-position %)])
               :on-wheel (goog.functions.throttle #(rf/dispatch [::events/scroll-graph %])
                                                  100)}
         [:rect {:class "graph__bg"
                 :height 10000
                 :width  10000
                 :on-mouse-down #(rf/dispatch [::events/mouse-down-graph-bg (local-position %)])}]
         ;; This is a bit sneaky. The program graph isn't included in the
         ;; initial render (when @program is nil), but the parent SVG is. This
         ;; is important because on the next render, when the graph is included,
         ;; it relies on the parent SVG already existing (so it can calculate
         ;; text element widths).
         (when @program
           [graph-viewport
            {:set-ref #(reset! !svg-viewport %)}
            (map-indexed (fn [idx]
                           [rule {:index  idx
                                  :local-position local-position
                                  :key    idx}])
                         (:rules @program))
            #_(doall
               (map (fn [[id _]]
                      [literal {:id id
                                :local-position local-position
                                :key id}])
                    (util/all-body-literals @program)))
            #_(map-indexed (fn [rule-idx _]
                             [composition-connectors
                              {:rule-index rule-idx
                               :key rule-idx}])
                           (:rules @program))
            (map (fn [match]
                   [match-connector
                    {:connection match
                     :key (str match)}])
                 matches)])]))))

(defn epilog-panel []
  (let [rules @(rf/subscribe [::subs/populated-rules])]
    [:div {:class "epilog-inspector"}
     [:pre {:class "code"}
      (string/join "\n\n" (map epilog/rule-to-epilog rules))]]))

(defn toolbar []
  (let [undos? @(rf/subscribe [:undos?])
        redos? @(rf/subscribe [:redos?])
        show-saved-popup? @(rf/subscribe [::subs/show-saved-popup?])]
    [:div {:class "toolbar"}
     (when show-saved-popup?
       [:div {:class "toolbar__saved-popup"}
        "Saved."])
     [:button {:on-click #(rf/dispatch [::events/save])}
      "Save"]
     [:button {:on-click #(rf/dispatch [:undo])
               :disabled (not undos?)}
      "Undo"]
     [:button {:on-click #(rf/dispatch [:redo])
               :disabled (not redos?)}
      "Redo"]]))

(defn main-panel []
  [:div {:id "app-container"}
   [:div {:class "work-viewport"}
    [program-graph]
    [:div {:class "inspectors"}
     [epilog-panel]]]
   [toolbar]])
