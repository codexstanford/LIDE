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

(defn localize-event-to-svg [^js svg event]
  (let [dom-point (js/DOMPoint. (.-clientX event) (.-clientY event))
        svg-point (.matrixTransform dom-point (.inverse (.getScreenCTM svg)))]
    {:x (.-x svg-point)
     :y (.-y svg-point)}))

(defn rule-view-model [highlighted-connection rule-index rule]
  (let [internals (util/internal-names rule)]
    {:head (:head rule)
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
  {:predicate (:predicate literal)
   :args      (:args literal)
   :highlight (filterv (fn [arg]
                         (some #(= % [literal-id arg])
                               (vals (select-keys highlighted-connection [:src :dest]))))
                       (:args literal))})

(defn literals-view-model [highlighted-connection program]
  (->> (:rules program)
       (mapv
        (fn [ref-rule]
          (mapv
           (fn [body-literal-id]
             (let [body-literal (get (:literals program) body-literal-id)]
               {body-literal-id (literal-view-model highlighted-connection
                                                    body-literal-id
                                                    body-literal)}))
           (:body ref-rule))))
       flatten
       (into {})))

(defn filter-by-head [head-literal program]
  (->> program
       (filter (fn [[id rule]]
                 (= (-> rule :head :predicate)
                    (-> head-literal :predicate))))
       ;; TODO filter by args as well?
       (into {})))

(defn compositions-view-model [program rule-idx rule highlighted-connection]
  (->> rule
       (util/compositions program)
       (mapv
        (fn [{:keys [literal-id arg]}]
          (let [unhighlighted
                ;; arg from composition should match an arg in rule (or be nil), so we
                ;; can use it on both ends of the connector
                {:src [rule-idx arg]
                 :dest [literal-id arg]}]
            (assoc unhighlighted
                   :highlighted
                   (= unhighlighted highlighted-connection)))))))

(defn groundings-view-model [program]
  "Find head literals that match with body literals from other rules."
  (->> (:rules program)
       (map-indexed
        (fn [rule-idx rule]
          (->> (:body rule)
               (mapv
                (fn [body-literal-id]
                  (let [body-literal (get (:literals program) body-literal-id)]
                    (->> (:rules program)
                         (map-indexed (fn [idx rule] [idx rule]))
                         (filter (fn [[_ grounding-rule]]
                                   (util/matches? body-literal (get (:literals program)
                                                                    (:head grounding-rule)))))
                         (map (fn [[grounding-rule-idx grounding-rule]]
                                {:src  [grounding-rule-idx (->> grounding-rule
                                                                :head
                                                                (get (:literals program))
                                                                :args)]
                                 :dest [body-literal-id (:args body-literal)]})))))))))
       flatten))

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

(defn literal [{:keys [id local-position]}]
  (let [literal-raw @(re-frame/subscribe [::subs/literal id])
        highlighted-connection @(re-frame/subscribe [::subs/highlighted-connection])
        literal-model (literal-view-model highlighted-connection id literal-raw)
        position @(re-frame/subscribe [::subs/literal-position id])
        layout (literal-layout literal-model position)]
    [:g {:on-mouse-down #(re-frame/dispatch [::events/start-drag-literal (local-position %) id])
         :on-click #(re-frame/dispatch [::events/select-literal id])
         :transform (str "translate(" (-> layout :container :position :x) "," (-> layout :container :position :y) ")")
         :key id}
     [:rect {:class  "rule__bg"
             :width  (->> layout :container :size :width)
             :height (->> layout :container :size :height)}]
     [util/eip-svg-text
      {:value (:predicate literal-model)
       :on-blur #(re-frame/dispatch [::events/edit-literal-predicate id (-> % .-target .-value)])
       :x rule-head-padding
       :y (/ (+ rule-head-padding rule-head-font-size rule-head-padding) 2)
       :width (->> layout :container :size :width)
       :height (+ rule-head-padding rule-head-font-size rule-head-padding)}]
     [:<>
      (map-indexed
       (fn [arg-index [arg arg-layout]]
         [util/eip-svg-text
          {:value arg
           :on-blur #(re-frame/dispatch [::events/edit-literal-arg id arg-index (-> % .-target .-value)])
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
             :on-click #(re-frame/dispatch [::events/add-literal-argument id])}
      "+ Add argument"]
     [:rect {:class  "rule__border"
             :width  (->> layout :container :size :width)
             :height (->> layout :container :size :height)}]]))

(defn rule-layout [rule position]
  (let [name-height (+ rule-head-padding
                       rule-head-font-size
                       rule-head-padding)

        predicate {:predicate (-> rule :head :predicate)
                   :position {:x rule-head-padding
                              :y (/ name-height 2)}
                   :size     {:width  150
                              :height name-height}}

        ;; Add Binding is also one arg tall
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
                                        (* i arg-height))}
                       :size {:width 150
                              :height arg-height}}])
                   (:internals rule))
        internals-height (* arg-height (->> rule :internals count))

        add-argument {:position {:y (+ name-height
                                       args-height
                                       internals-height
                                       (/ arg-height 2))}}

        add-body-literal {:position {:y (+ name-height
                                           args-height
                                           internals-height
                                           arg-height
                                           (/ arg-height 2))}}]
    {:predicate predicate
     :args (into {} args)
     :internals (into {} internals)
     :add-argument add-argument
     :add-body-literal add-body-literal
     :container {:position (or position {:x 0 :y 0})
                 :size {:width  150
                        :height (+ name-height
                                   args-height
                                   arg-height
                                   internals-height
                                   arg-height)}}}))

(defn rule [{:keys [index local-position]}]
  ;; TODO Should get layout data for EIPs from layout object
  (let [rule-raw @(re-frame/subscribe [::subs/populated-rule index])
        highlighted-connection @(re-frame/subscribe [::subs/highlighted-connection])
        rule-model (rule-view-model highlighted-connection index rule-raw)
        position @(re-frame/subscribe [::subs/rule-position index])
        layout (rule-layout rule-model position)]
    [:g {:on-mouse-down #(re-frame/dispatch [::events/start-drag-rule (local-position %) index])
         :on-click #(re-frame/dispatch [::events/select-rule index])
         :transform (str "translate(" (-> layout :container :position :x) "," (-> layout :container :position :y) ")")
         :key index}
     [:rect {:class  "rule__bg"
             :width  (->> layout :container :size :width)
             :height (->> layout :container :size :height)}]
     [util/eip-svg-text
      {:value (-> rule-model :head :predicate)
       :on-blur #(re-frame/dispatch [::events/edit-head-predicate index (-> % .-target .-value)])
       :x rule-head-padding
       :y (/ (+ rule-head-padding rule-head-font-size rule-head-padding) 2)
       :width  (->> layout :container :size :width)
       :height (+ rule-head-padding rule-head-font-size rule-head-padding)}]
     [:<>
      (map-indexed
       (fn [arg-index [arg arg-layout]]
         [util/eip-svg-text
          {:value arg
           :on-blur #(re-frame/dispatch [::events/edit-head-arg index arg-index (-> % .-target .-value)])
           :x rule-binding-padding-x
           :y (->> arg-layout :position :y)
           :width  (->> layout :container :size :width)
           :height (+ rule-head-padding rule-head-font-size rule-head-padding)
           :display-class (if (some #(= % arg) (:highlight rule-model)) "rule--highlight" "")
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
             :on-click #(re-frame/dispatch [::events/add-argument index])}
      "+ Add argument"]
     [:text {:class "rule__add-arg"
             :x rule-binding-padding-x
             :y (->> layout :add-body-literal :position :y)
             :on-click #(re-frame/dispatch [::events/add-body-literal index])}
      "+ Add subgoal"]
     [:rect {:class  "rule__border"
             :width  (->> layout :container :size :width)
             :height (->> layout :container :size :height)}]]))

(defn socket-position [layout arg {:keys [end]}]
  (let [all-names (merge (:args layout)
                         (:internals layout))]
    {:x (+ (-> layout :container :position :x)
           (if (= end :dest)
             (-> layout :container :size :width)
             0))
     :y (+ (-> layout :container :position :y)
           (if (= arg :unbound)
             (/ rule-head-height 2)
             (-> all-names (get arg) :position :y)))}))

(defn composition-connector [connection]
  (let [[start-rule-idx start-arg] (:src connection)
        [end-literal-id end-arg]   (:dest connection)
        start-rule @(re-frame/subscribe [::subs/populated-rule start-rule-idx])
        start-rule-position @(re-frame/subscribe [::subs/rule-position start-rule-idx])
        end-literal @(re-frame/subscribe [::subs/literal end-literal-id])
        end-literal-position @(re-frame/subscribe [::subs/literal-position end-literal-id])
        highlighted-connection @(re-frame/subscribe [::subs/highlighted-connection])
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
             :on-mouse-over #(re-frame/dispatch [::events/highlight-connection (select-keys connection [:src :dest])])
             :on-mouse-leave #(re-frame/dispatch [::events/stop-connection-highlight])}]]))

(defn composition-connectors [{:keys [rule-index]}]
  (let [program @(re-frame/subscribe [::subs/program])
        rule @(re-frame/subscribe [::subs/rule rule-index])
        highlighted-conn @(re-frame/subscribe [::subs/highlighted-connection])
        compositions-model (compositions-view-model program rule-index rule highlighted-conn)]
    (into [:<>] (mapv (fn [cm]
                        [composition-connector cm])
                      compositions-model))))

(defn grounding-connector [{:keys [connection literal-layouts rule-layouts]}]
  ;; n.b. We expect start-args and end-args to have the same length.
  (let [[start-rule-idx start-args] (:src connection)
        [end-literal-id end-args]   (:dest connection)
        start-layout (get rule-layouts start-rule-idx)
        end-layout   (get literal-layouts end-literal-id)

        draw-connector
        (fn [start end]
          [:<>
           [:line {:x1 (:x start)
                   :y1 (:y start)
                   :x2 (:x end)
                   :y2 (:y end)
                   :stroke-dasharray "5,5"
                   :stroke (if (:highlighted connection) "green" "black")}]
           [:line {:x1 (:x start)
                   :y1 (:y start)
                   :x2 (:x end)
                   :y2 (:y end)
                   :stroke "transparent"
                   :stroke-width 10
                   :on-mouse-over #(re-frame/dispatch [::events/highlight-connection (select-keys connection [:src :dest])])
                   :on-mouse-leave #(re-frame/dispatch [::events/stop-connection-highlight])
                   :on-click #(re-frame/dispatch [::events/disconnect connection])}]])]
    (if (empty? start-args)
      (draw-connector (socket-position start-layout :unbound {:end :src})
                      (socket-position end-layout   :unbound {:end :dest}))
      (into
       [:<>]
       (mapv
        (fn [start-arg end-arg]
          (let [start (socket-position start-layout start-arg {:end :src})
                end (socket-position end-layout end-arg {:end :dest})]
            (draw-connector start end)))
        start-args
        end-args)))))

(defn graph-viewport [{:keys [set-ref]} & children]
  (let [graph-transform @(re-frame/subscribe [::subs/graph-transform])]
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

            program (re-frame/subscribe [::subs/program])
            rule-positions (re-frame/subscribe [::subs/rule-positions])
            literal-positions (re-frame/subscribe [::subs/literal-positions])
            highlighted-connection (re-frame/subscribe [::subs/highlighted-connection])
            literals-vm (literals-view-model @highlighted-connection @program)
            literal-layouts (->> literals-vm
                                 (map
                                  (fn [[id literal]]
                                    [id (literal-layout literal (get @literal-positions id))]))
                                 (into {}))
            rules-vm (rules-view-model @highlighted-connection @program)
            rule-layouts (->> rules-vm
                              (map-indexed
                               (fn [idx rule]
                                 [idx (rule-layout rule (get @rule-positions idx))]))
                              (into {}))
            groundings-vm (groundings-view-model @program)]
        [:svg {:class "graph-panel"
               :on-mouse-move (goog.functions.throttle #(re-frame/dispatch [::events/mouse-move (local-position %)])
                                                       25)
               :on-mouse-up #(re-frame/dispatch [::events/mouse-up (local-position %)])
               :on-wheel (goog.functions.throttle #(re-frame/dispatch [::events/scroll-graph %])
                                                  100)}
         [:rect {:class "graph__bg"
                 :height 10000
                 :width  10000
                 :on-mouse-down #(re-frame/dispatch [::events/mouse-down-graph-bg (local-position %)])}]
         [graph-viewport
          {:set-ref #(reset! !svg-viewport %)}
          (map-indexed (fn [idx]
                         [rule {:index  idx
                                :local-position local-position
                                :key    idx}])
                       (:rules @program))
          (doall
           (map (fn [[id _]]
                  [literal {:id id
                            :local-position local-position
                            :key id}])
                (util/all-body-literals @program)))
          (map-indexed (fn [rule-idx _]
                         [composition-connectors
                          {:rule-index rule-idx
                           :key rule-idx}])
                       (:rules @program))
          (map (fn [vm]
                 [grounding-connector
                  {:connection vm
                   :literal-layouts literal-layouts
                   :rule-layouts rule-layouts
                   :key (str vm)}])
               groundings-vm)]]))))

(defn epilog-panel []
  (let [rules @(re-frame/subscribe [::subs/populated-rules])]
    [:div {:class "epilog-inspector"}
     [:pre {:class "code"}
      (string/join "\n\n" (map epilog/rule-to-epilog rules))]]))

(defn toolbar []
  (let [undos? @(re-frame/subscribe [:undos?])
        redos? @(re-frame/subscribe [:redos?])
        show-saved-popup? @(re-frame/subscribe [::subs/show-saved-popup?])]
    [:div {:class "toolbar"}
     (when show-saved-popup?
       [:div {:class "toolbar__saved-popup"}
        "Saved."])
     [:button {:on-click #(re-frame/dispatch [::events/save])}
      "Save"]
     [:button {:on-click #(re-frame/dispatch [:undo])
               :disabled (not undos?)}
      "Undo"]
     [:button {:on-click #(re-frame/dispatch [:redo])
               :disabled (not redos?)}
      "Redo"]]))

(defn main-panel []
  (when @(re-frame/subscribe [::subs/program])
    [:div {:id "app-container"}
     [:div {:class "work-viewport"}
      [program-graph]
      [:div {:class "inspectors"}
       [epilog-panel]]]
     [toolbar]]))
