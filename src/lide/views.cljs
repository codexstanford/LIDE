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

(defn socket [props]
  [:div (merge props {:class "socket"})])

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

(defn body-literal-html [{:keys [id]}]
  (let [literal @(rf/subscribe [::subs/literal id])]
    [:div {:class "body-literal"
           :data-literal-id id}
     [:div {:class "body-literal__predicate"}
      [util/eip-plain-text
       {:value (:predicate literal)
        :on-blur #(rf/dispatch [::events/edit-literal-predicate id (-> % .-target .-value)])}]
      [socket]]
     [:div {:class "rule__tutor"} "is true of..."]
     [:<>
      (map-indexed
       (fn [arg-index arg]
         [util/eip-plain-text
          {:value arg
           :on-blur #(rf/dispatch [::events/edit-literal-arg id arg-index (-> % .-target .-value)])
           :style (when (util/variable? arg) {"fill" (util/hash-to-hsl arg)})
           :key arg}])
       (:args literal))]
     [:div {:class "button-add"
            :on-click #(rf/dispatch [::events/add-literal-argument id])}
      "+ and..."]]))

(defn rule [{:keys [id local-position]}]
  (let [rule @(rf/subscribe [::subs/populated-rule id])
        position @(rf/subscribe [::subs/position :rule id])]
    [:foreignObject {:width 1
                     :height 1
                     :style {"overflow" "visible"}
                     :transform (str "translate(" (:x position) ", " (:y position) ")")}
     [:div {:class "rule__wrapper"
            :on-mouse-down #(rf/dispatch [::events/start-drag-rule (local-position %) id])}
      [:div {:class "rule"
             :ref #(rf/dispatch [::events/rendered :rule id %])}
       [:div {:class "rule__head-predicate"}
        [socket {:on-click #(rf/dispatch [::events/select-defeater id])}]
        [util/eip-plain-text
         {:value (-> rule :head :predicate)
          :on-blur #(rf/dispatch [::events/edit-head-predicate id (-> % .-target .-value)])}]
        [socket]]
       (if (seq (-> rule :head :args))
         [:<>
          [:div {:class "rule__tutor"} "is true of..."]
          [:<>
           (map-indexed
            (fn [arg-index arg]
              [util/eip-plain-text
               {:value arg
                :on-blur #(rf/dispatch [::events/edit-head-arg id arg-index (-> % .-target .-value)])
                ;; XXX style isn't updating properly on change
                :style (when (util/variable? arg) {"color" (util/hash-to-hsl arg)})
                :key arg-index}])
            (-> rule :head :args))]
          [:div {:class "rule__add-arg button-add"
                 :on-click #(rf/dispatch [::events/add-argument id])}
           "+ and..."]]
         [:div {:class "rule__add-arg button-add"
                :on-click #(rf/dispatch [::events/add-argument id])}
          "+ is true of..."])
       (if (seq (:body rule))
         [:<>
          [:div {:class "rule__tutor"} "when..."]
          [:<>
           (map
            (fn [literal]
              [body-literal-html {:id (:id literal)
                                  :key (:id literal)}])
            (:body rule))]
          [:div {:class "rule__add-arg button-add"
                 :on-click #(rf/dispatch [::events/add-body-literal id])}
           "+ and..."]]
         [:div {:class "rule__add-arg button-add"
                :on-click #(rf/dispatch [::events/add-body-literal id])}
          "+ when..."])
       [:div {:class "rule__add-arg button-add"
              :on-click #(rf/dispatch [::events/defeated-selecting-defeater id])}
        "+ unless..."]]]]))

(defn fact [{:keys [id localize-position]}]
  (let [fact @(rf/subscribe [::subs/fact id])
        position @(rf/subscribe [::subs/position :fact id])]
    [:foreignObject {:width 1
                     :height 1
                     :style {"overflow" "visible"}
                     :transform (str "translate(" (:x position) ", " (:y position) ")")}
     [:div
      {:class "fact__wrapper"
       :on-mouse-down #(rf/dispatch [::events/start-drag-fact (localize-position %) id])}
      [:div
       {:class "fact"
        :ref #(rf/dispatch [::events/rendered :fact id %])}
       [:div {:class "fact__name"} (:name fact)]
       [:div {:class "fact__attributes"}
        (map
         (fn [[attr-name attr-value]]
           [:div {:class "fact__attribute"
                  :data-attribute-name attr-name
                  :key attr-name}
            [:div {:class "fact__attribute-name fact__attribute-cell"}
             attr-name]
            [:div {:class "fact__attribute-value fact__attribute-cell"}
             (condp = (:type attr-value)
               :primitive (:value attr-value)
               :subobject [socket])]])
         (:attributes fact))]]]]))

(defn subobject-connector [{:keys [fact-id attribute-name subobject-id]}]
  (let [fact-layout @(rf/subscribe [::subs/layout :fact fact-id])
        subobject-layout @(rf/subscribe [::subs/layout :fact subobject-id])
        attribute-layout (get-in fact-layout [:attributes attribute-name])
        ;; TODO Fix magic numbers, just need to get more information from layout
        start (merge-with +
                          (-> fact-layout :container :position)
                          (-> attribute-layout :position)
                          {:y (/ (-> attribute-layout :size :height) 2)}
                          {:x (-> fact-layout :container :size :width)}
                          {:x -15})
        end (merge-with +
                        (-> subobject-layout :container :position)
                        {:y 15})]
    [:line {:x1 (:x start)
            :y1 (:y start)
            :x2 (:x end)
            :y2 (:y end)
            :stroke "black"}]))

(defn subobject-connectors [{:keys [fact-id]}]
  (let [fact @(rf/subscribe [::subs/fact fact-id])
        layout @(rf/subscribe [::subs/layout :fact fact-id])]
    [:<>
     (->> (:attributes fact)
          (mapv
           (fn [[attr-name attr-val]]
             (when (= :subobject (:type attr-val))
               [subobject-connector {:fact-id fact-id
                                     :attribute-name attr-name
                                     :subobject-id (:value attr-val)
                                     :key fact-id}])))
          (remove nil?))]))

(defn socket-position [rule-layout literal-id {:keys [end]}]
  "Find the XY location where a connector should terminate on a particular rule
  and, optionally, body literal."
  (merge-with +
              (-> rule-layout :container :position)
              ;; XXX more magic numbers here
              {:x (cond
                    ;; Two concerns here: `end` being :start or :dest determines
                    ;; which side of the rule the socket is on, and if
                    ;; `literal-id` is non-nil the socket is slightly inset.
                    (and (= end :dest) (not= literal-id :unbound))
                    (- (-> rule-layout :container :size :width) 20)

                    (and (= end :dest) (= literal-id :unbound))
                    (- (-> rule-layout :container :size :width) 9)

                    (and (= end :start) (not= literal-id :unbound))
                    10

                    :else
                    10)
               :y (if (= literal-id :unbound)
                    0
                    (get-in rule-layout [:body literal-id :position :y]))}
              {:y 10}))

(defn rule-match-connector [{:keys [connection]}]
  "Draw a line connecting :src and :dest of `connection`."
  (let [[end-rule-id end-literal-id] (:dest connection)
        start-layout @(rf/subscribe [::subs/layout :rule (:src connection)])
        end-layout   @(rf/subscribe [::subs/layout :rule end-rule-id])
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
             :on-mouse-leave #(rf/dispatch [::events/stop-connection-highlight])}]]))

(defn defeat-connector [{:keys [defeat]}]
  "Draw a line connecting the defeated and defeater rules from `defeat`."
  (let [{:keys [defeated defeater]} defeat
        start-layout @(rf/subscribe [::subs/layout :rule defeater])
        end-layout   @(rf/subscribe [::subs/layout :rule defeated])
        start (socket-position start-layout :unbound {:end :src})
        end   (socket-position end-layout :unbound {:end :dest})]
    [:<>
     [:line {:x1 (:x start)
             :y1 (:y start)
             :x2 (:x end)
             :y2 (:y end)
             :stroke "red"}]
     [:line {:x1 (:x start)
             :y1 (:y start)
             :x2 (:x end)
             :y2 (:y end)
             :class "defeat-connector__clickable"
             :stroke "transparent"
             :stroke-width 10
             :on-click #(rf/dispatch [::events/remove-defeat defeat])}]]))

(defn defeat-connector-pending [{:keys [mouse-position]}]
  "Draw a line emanating from the defeated rule while we select another rule to be defeater."
  (let [defeated-id @(rf/subscribe [::subs/defeated-selecting-defeater])
        defeated-layout @(rf/subscribe [::subs/layout :rule defeated-id])
        start (socket-position defeated-layout :unbound {:end :dest})]
    (when defeated-id
      [:line {:class "defeat-connector"
              :x1 (:x start)
              :y1 (:y start)
              :x2 (:x mouse-position)
              :y2 (:y mouse-position)}])))

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
            rule-matches @(rf/subscribe [::subs/rule-matches])
            defeatings @(rf/subscribe [::subs/defeatings])
            mouse-position @(rf/subscribe [::subs/mouse-position])]
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
            (map (fn [[id _]]
                   [:<> {:key id}
                    [fact {:id id
                           :localize-position local-position}]
                    [subobject-connectors {:fact-id id}]])
                 (:facts program))
            (map (fn [[id _]]
                   [rule {:id  id
                          :local-position local-position
                          :key    id}])
                 (:rules program))
            (map (fn [match]
                   [rule-match-connector
                    {:connection match
                     :key (str match)}])
                 rule-matches)
            (map (fn [defeat]
                   [defeat-connector
                    {:defeat defeat
                     :key (str defeat)}])
                 defeatings)
            [defeat-connector-pending {:key "defeat-pending"
                                       :mouse-position mouse-position}]]])))))

(defn epilog-panel []
  (let [rules @(rf/subscribe [::subs/populated-rules])
        defeatings @(rf/subscribe [::subs/defeatings])
        facts @(rf/subscribe [::subs/facts])]
    [:div {:class "epilog-inspector"}
     [:pre {:class "code"}
      "#########\n# Rules #\n#########\n\n"
      (->> rules
           (map
            (fn [[id rule]]
              (epilog/rule-to-epilog
               rule
               (->> defeatings
                    (filter #(= id (:defeated %)))
                    (mapv #(get rules (:defeater %)))))))
           (string/join "\n\n"))
      "\n\n#########\n# Facts #\n#########\n\n"
      (->> facts
           (map
            (fn [[id fact]]
              (epilog/fact-to-epilog facts id fact)))
           (string/join "\n\n"))]]))

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
