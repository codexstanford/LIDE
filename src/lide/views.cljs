(ns lide.views
  (:require
   [clojure.string :as string]
   [re-frame.core :as rf]
   [lide.epilog :as epilog]
   [lide.events :as events]
   [lide.graph :as graph]
   [lide.subs :as subs]
   [lide.util :as util]
   ))

(defn center-position [{:keys [position size]}]
  "Find the center point of a square at `position` with `size`."
  {:x (+ (:x position) (/ (:width size) 2))
   :y (+ (:y position) (/ (:height size) 2))})

(defn localize-event-to-svg [^js svg event]
  "Find the coordinates of `event` in `svg`'s local coordinate space."
  (let [dom-point (js/DOMPoint. (.-clientX event) (.-clientY event))
        svg-point (.matrixTransform dom-point (.inverse (.getScreenCTM svg)))]
    {:x (.-x svg-point)
     :y (.-y svg-point)}))

(defn literal-collapse [id layout]
  (let [symbol  (if (:collapsed layout) "+" "-")
        tooltip (if (:collapsed layout) "Expand literal" "Collapse literal")]
    [:<>
     [:rect {:class "rule__button-bg"
             :x (- (-> layout :container :size :width) 20)
             :y 0
             :height graph/rule-head-height
             :width 20
             :on-click #(rf/dispatch [::events/toggle-collapse-literal id])}
      [:title tooltip]]
     [:text {:class "rule__button-label"
             :x (- (-> layout :container :size :width) 15)
             :y (/ graph/rule-head-height 2)}
      symbol]]))

(defn literal-negate [id layout]
  [:<>
   [:rect {:class "rule__button-bg"
           :x (- (-> layout :container :size :width) 40)
           :y 0
           :height graph/rule-head-height
           :width 20
           :on-click #(rf/dispatch [::events/negate-literal id])}
    [:title "Negate literal"]]
   [:text {:class "rule__button-label"
           :x (- (-> layout :container :size :width) 35)
           :y (/ graph/rule-head-height 2)}
    "~"]])

(defn body-literal-collapsed [{:keys [layout]}]
  (let [id (:id layout)]
    [:g {:transform (str "translate(" (-> layout :container :position :x) "," (-> layout :container :position :y) ")")}
     [:rect {:class  "rule__bg"
             :width  (->> layout :container :size :width)
             :height (->> layout :container :size :height)}]
     [:text
      {:x graph/rule-head-padding
       :y (/ (->> layout :container :size :height) 2)
       :width (->> layout :container :size :width)
       :height (->> layout :container :size :height)}
      (-> layout :predicate :predicate)]
     [:<>
      (map-indexed
       (fn [arg-index [arg arg-layout]]
         [:text
          {:x (->> arg-layout :position :x)
           :y (->> arg-layout :position :y)
           :width  (->> arg-layout :size :width)
           :height (->> layout :container :size :height)
           :style (when (util/variable? arg) {"fill" (util/hash-to-hsl arg)})
           :key arg-index}
          arg])
       (:args layout))]
     [literal-collapse id layout]
     [:rect {:class  "rule__border"
             :width  (->> layout :container :size :width)
             :height (->> layout :container :size :height)}]]))

(defn body-literal-uncollapsed [{:keys [layout]}]
  (let [id (:id layout)
        highlighted-connection @(rf/subscribe [::subs/highlighted-connection])]
    [:g {:transform (str "translate(" (-> layout :container :position :x) "," (-> layout :container :position :y) ")")}
     [:rect {:class  "rule__bg"
             :width  (->> layout :container :size :width)
             :height (->> layout :container :size :height)}]
     [util/eip-svg-text
      {:value (-> layout :predicate :predicate)
       :on-blur #(rf/dispatch [::events/edit-literal-predicate id (-> % .-target .-value)])
       :x graph/rule-head-padding
       :y (/ (+ graph/rule-head-padding graph/rule-head-font-size graph/rule-head-padding) 2)
       :width (->> layout :container :size :width)
       :height (+ graph/rule-head-padding graph/rule-head-font-size graph/rule-head-padding)}]
     [literal-negate id layout]
     [literal-collapse id layout]
     [:<>
      (map-indexed
       (fn [arg-index [arg arg-layout]]
         [util/eip-svg-text
          {:value arg
           :on-blur #(rf/dispatch [::events/edit-literal-arg id arg-index (-> % .-target .-value)])
           :x graph/rule-binding-padding-x
           :y (->> arg-layout :position :y)
           :width  (->> layout :container :size :width)
           :height (+ graph/rule-head-padding graph/rule-head-font-size graph/rule-head-padding)
           :display-style (when (util/variable? arg) {"fill" (util/hash-to-hsl arg)})
           :key arg-index}])
       (:args layout))]
     [:text {:class "rule__add-arg"
             :x graph/rule-binding-padding-x
             :y (->> layout :add-argument :position :y)
             :on-click #(rf/dispatch [::events/add-literal-argument id])}
      "+ Add argument"]
     [:rect {:class  "rule__border"
             :width  (->> layout :container :size :width)
             :height (->> layout :container :size :height)}]]))

(defn body-literal [{:keys [layout] :as props}]
  (if (:collapsed layout)
    [body-literal-collapsed   props]
    [body-literal-uncollapsed props]))

(defn rule [{:keys [index local-position]}]
  ;; TODO Should get layout data for EIPs from layout object
  (let [rule-raw @(rf/subscribe [::subs/populated-rule index])
        highlighted-connection @(rf/subscribe [::subs/highlighted-connection])
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
      {:value (-> rule-raw :head :predicate)
       :on-blur #(rf/dispatch [::events/edit-head-predicate index (-> % .-target .-value)])
       :x graph/rule-head-padding
       :y (/ (+ graph/rule-head-padding graph/rule-head-font-size graph/rule-head-padding) 2)
       :width  (->> layout :container :size :width)
       :height (+ graph/rule-head-padding graph/rule-head-font-size graph/rule-head-padding)}]
     [:<>
      (map-indexed
       (fn [arg-index [arg arg-layout]]
         [util/eip-svg-text
          {:value arg
           :on-blur #(rf/dispatch [::events/edit-head-arg index arg-index (-> % .-target .-value)])
           :x graph/rule-binding-padding-x
           :y (->> arg-layout :position :y)
           :width  (->> layout :container :size :width)
           :height (+ graph/rule-head-padding graph/rule-head-font-size graph/rule-head-padding)
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
           {:x graph/rule-binding-padding-x
            :y (->> arg-layout :position :y)}
           arg]])
       (:internals layout))]
     [:text {:class "rule__add-arg"
             :x graph/rule-binding-padding-x
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
             :x graph/rule-binding-padding-x
             :y (->> layout :add-body-literal :position :y)
             :on-click #(rf/dispatch [::events/add-body-literal index])}
      "+ Add subgoal"]
     [:rect {:class  "rule__border"
             :width  (->> layout :container :size :width)
             :height (->> layout :container :size :height)}]]))

(defn socket-position [rule-layout literal-id {:keys [end]}]
  "Find the XY location where a connector should terminate on a particular rule
  and, optionally, body literal."
  {:x (+ (-> rule-layout :container :position :x)
         ;; Two concerns here: `end` being :start or :dest determines which side
         ;; of the rule the socket is on, and if `literal-id` is non-nil the
         ;; socket is slightly inset.
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
         (/ graph/rule-head-height 2)
         (if (= literal-id :unbound)
           0
           (get-in rule-layout [:body literal-id :container :position :y])))})

(defn match-connector [{:keys [connection]}]
  "Draw a line connecting :src and :dest of `connection`."
  (let [[end-rule-idx end-literal-id] (:dest connection)
        start-layout @(rf/subscribe [::subs/rule-layout (:src connection)])
        end-layout   @(rf/subscribe [::subs/rule-layout end-rule-idx])
        start (socket-position start-layout :unbound {:end :src})
        end   (socket-position end-layout end-literal-id {:end :dest})]
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
             :on-click #(rf/dispatch [::events/disconnect connection])}]]))

(defn graph-viewport [{:keys [set-ref]} & children]
  "Draw an SVG group to contain the program graph.

  Using a group is useful because we can apply scale/translation
  transformations to the entire graph at once."
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

            program @(rf/subscribe [::subs/program])
            matches @(rf/subscribe [::subs/matches])]
        (when program
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
           [graph-viewport
            {:set-ref #(reset! !svg-viewport %)}
            (map-indexed (fn [idx]
                           [rule {:index  idx
                                  :local-position local-position
                                  :key    idx}])
                         (:rules program))
            (map (fn [match]
                   [match-connector
                    {:connection match
                     :key (str match)}])
                 matches)]])))))

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
