(ns lide.epilog.views
  (:require
   [clojure.string :as string]
   [re-frame.core :as rf]
   [lide.epilog.core :as epilog]
   [lide.events :as events]
   [lide.subs :as subs]
   [lide.util :as util]
   [lide.views :as views]
   [lide.epilog.events :as el-events]
   [lide.epilog.subs :as el-subs]))

#_(defn literal-collapse [id layout]
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

(defn body-literal [{:keys [literal]}]
  [:div {:class "body-literal"}
   [:div {:class "body-literal__predicate"}
    [:span {:on-click #(rf/dispatch [::events/select-range
                                     (-> literal :predicate :startPosition)
                                     (-> literal :predicate :endPosition)])}
     (-> literal :predicate :text)]
    [:button {:title "Negate"
              :class "rule__button"
              :on-click #(rf/dispatch [::el-events/negate-literal (:startPosition literal)])}
     "not"]
    [views/socket]]
   (if (seq (:args literal))
     [:<>
      [:div {:class "rule__tutor"}
       "is "
       (when (:negative literal) [:span {:class "rule__tutor--stress"} "not "])
       "true of..."]
      (map-indexed
       (fn [arg-idx arg]
         [:span {:on-click #(rf/dispatch [::events/select-range
                                          (:startPosition arg)
                                          (:endPosition arg)])
                 :key arg-idx}
          (:text arg)])
       (:args literal))
      [:div {:class "button-add"
             :on-click #(rf/dispatch [::events/add-literal-argument])}
       "+ and..."]]

     [:div {:class "button-add"
            :on-click #(rf/dispatch [::events/add-literal-argument])}
      "+ is true of..."])])

(defn fact [{:keys [id localize-position]}]
  (let [fact @(rf/subscribe [::subs/fact id])
        position @(rf/subscribe [::subs/position id])]
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
       [:div {:class "fact__name"} (:type fact)]
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
               :subobject [views/socket])]])
         (:attributes fact))]]]]))

(defn rule-html [{:keys [id localize-position store-ref]}]
  (let [rule-instances @(rf/subscribe [::el-subs/rule id])]
    [:div {:class "rule"
           :ref store-ref
           :on-mouse-down #(rf/dispatch [::events/start-drag (localize-position %) id :rule])}
     [:div {:class "rule__head-predicate"}
      [views/socket {:on-click #(rf/dispatch [::events/select-defeater id])}]
      [:span {:on-click #(rf/dispatch [::events/select-range
                                       (get-in rule-instances [0 :head :predicate :startPosition])
                                       (get-in rule-instances [0 :head :predicate :endPosition])])}
       (get-in rule-instances [0 :head :predicate :text])]]
     (map-indexed
      (fn [idx rule-instance]
        [:<> {:key idx}
         (if (seq (-> rule-instance :head :args))
           [:<>
            [:div {:class "rule__tutor"} "is true of..."]
            (map-indexed
             (fn [arg-idx arg]
               [:span {:on-click #(rf/dispatch [::events/select-range
                                                (:startPosition arg)
                                                (:endPosition   arg)])
                       :key arg-idx}
                (:text arg)])
             (-> rule-instance :head :args))
            ;; TODO fix adding arguments
            [:div {:class "rule__add-arg button-add"
                   :on-click #(rf/dispatch [::events/add-argument id])}
             "+ and..."]]
           [:div {:class "rule__add-arg button-add"
                  :on-click #(rf/dispatch [::events/add-argument id])}
            "+ is true of..."])
         (if (seq (:body rule-instance))
           [:<>
            [:div {:class "rule__tutor"} "when..."]
            (map-indexed
             (fn [literal-idx literal]
               [body-literal {:literal literal
                              :key literal-idx}])
             (:body rule-instance))
            [:div {:class "rule__add-arg button-add"
                   :on-click #(rf/dispatch [::events/add-body-literal id])}
             "+ and..."]]
           [:div {:class "rule__add-arg button-add"
                  :on-click #(rf/dispatch [::events/add-body-literal id])}
            "+ when..."])
         [:div {:class "rule__add-defeater button-add"
                :on-click #(rf/dispatch [::events/defeated-selecting-defeater id])}
          [:div {:class "rule__add-defeater-label"} "+ unless..."]
          [views/socket]]])
      rule-instances)]))

;; when... unless... or when... unless... or when... unless...

(defn rule [{:keys [id] :as props}]
  [views/prerender {:element-type :rule
                    :id id}
   [rule-html props]])

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

(defn program-graph [{:keys [localize-position]}]
  (let [program @(rf/subscribe [::subs/program])
        rule-matches @(rf/subscribe [::subs/rule-matches])
        defeatings @(rf/subscribe [::subs/defeatings])
        mouse-position @(rf/subscribe [::subs/mouse-position])]
    [:<>
     (map (fn [[id _]]
            [:<> {:key id}
             [fact {:id id
                    :localize-position localize-position}]
             [subobject-connectors {:fact-id id}]])
          (:facts program))
     (map (fn [[name _]]
            [rule {:id name
                   :localize-position localize-position
                   :key name}])
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
                                :mouse-position mouse-position}]]))

(defn code-panel []
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
