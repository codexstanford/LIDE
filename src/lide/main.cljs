(ns lide.main
  (:require
   [clojure.string :as string]
   [re-frame.core :as rf]
   [lide.events :as events]
   [lide.subs :as subs]
   [lide.util :as util]
   [lide.views :as views]
   [lide.epilog.core :as epilog]
   [lide.epilog.views :as epilog-views]))

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
                    (- (-> rule-layout :container :size :width) 17)

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
        end   (merge-with +
                          (socket-position end-layout :unbound {:end :dest})
                          {:y (-> end-layout :container :size :height)}
                          {:y -20})]
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
        start (merge-with +
                          (socket-position defeated-layout :unbound {:end :dest})
                          {:y (-> defeated-layout :container :size :height)}
                          {:y -20})]
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
      (let [localize-position (fn [event]
                                (views/localize-event-to-svg @!svg-viewport event))

            program @(rf/subscribe [::subs/program])
            rule-matches @(rf/subscribe [::subs/rule-matches])
            defeatings @(rf/subscribe [::subs/defeatings])
            mouse-position @(rf/subscribe [::subs/mouse-position])]
        (when program
          [:svg {:class "graph-panel"
                 :id "graph-svg"
                 :on-mouse-move (goog.functions.throttle
                                 #(rf/dispatch [::events/mouse-move (localize-position %)])
                                 25)
                 :on-mouse-up #(rf/dispatch [::events/mouse-up (localize-position %)])
                 :on-wheel (goog.functions.throttle #(rf/dispatch [::events/scroll-graph %])
                                                    100)}
           [:rect {:class "graph__bg"
                   :height 10000
                   :width  10000
                   :on-mouse-down #(rf/dispatch [::events/mouse-down-graph-bg (localize-position %)])}]
           [graph-viewport
            {:set-ref #(reset! !svg-viewport %)}
            (map (fn [[id _]]
                   [:<> {:key id}
                    [epilog-views/fact {:id id
                                        :localize-position localize-position}]
                    [subobject-connectors {:fact-id id}]])
                 (:facts program))
            (map (fn [[id _]]
                   [epilog-views/rule {:id  id
                                       :localize-position localize-position
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
        facts @(rf/subscribe [::subs/facts])

        compiled-rules
        (->> rules
             (map
              (fn [[id rule]]
                (epilog/compile-rule rule
                                     (->> defeatings
                                          (filter #(= id (:defeated %)))
                                          (mapv #(get rules (:defeater %))))))))]
    [:div {:class "epilog-inspector"}
     [:pre {:class "code"}
      "#########\n# Rules #\n#########\n\n"
      (string/join "\n\n" (map epilog/stringify-rule compiled-rules))
      "\n\n#########\n# Facts #\n#########\n\n"
      (->> facts
           (map
            (fn [[id fact]]
              (epilog/stringify-fact facts id fact)))
           (string/join "\n\n"))
      "\n\n########################\n# Converse productions #\n########################\n\n"
      (string/join "\n\n" (map epilog/stringify-converse-operation compiled-rules))]]))


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
