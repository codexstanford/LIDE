(ns lide.epilog.views
  (:require
   [clojure.string :as string]
   [re-frame.core :as rf]
   [lide.epilog.core :as epilog]
   [lide.editor :as editor]
   [lide.events :as events]
   [lide.subs :as subs]
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

(defn body-literal [{:keys [literal source-index]}]
  [:div {:class "body-literal"
         :data-source-index source-index}
   [:div {:class "body-literal__predicate"}
    [:span {:class "body-literal__predicate-text"
            :on-click #(rf/dispatch [::editor/focus-range
                                     [(-> literal :predicate :startPosition)
                                      (-> literal :predicate :endPosition)]])}
     (-> literal :predicate :text)]
    [:button {:title "Negate"
              :class "rule__button"
              :on-click #(rf/dispatch [::el-events/negate-literal
                                       (-> literal :predicate :startPosition)])}
     "~"]
    [views/socket]]
   (if (seq (:args literal))
     [:<>
      [:div {:class "rule__tutor"}
       "is "
       (when (:negative literal) [:span {:class "rule__tutor--stress"} "not "])
       "true of..."]
      (map-indexed
       (fn [arg-idx arg]
         [:div {:on-click #(rf/dispatch [::editor/focus-range
                                         [(:startPosition arg)
                                          (:endPosition arg)]])
                :key arg-idx}
          (:text arg)])
       (:args literal))])])

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

(defn rule-html [{:keys [path localize-position store-ref]}]
  (let [rule @(rf/subscribe [::el-subs/rule path])
        query-result @(rf/subscribe [::el-subs/query-result (first path)])]
    [:div {:class "rule"
           :ref store-ref
           :on-mouse-down #(rf/dispatch [::events/start-drag
                                         (localize-position %)
                                         path
                                         :rule])}
     [:div {:class "rule__head-predicate"}
      [views/socket {:on-click #(rf/dispatch [::events/select-defeater path])}]
      [:span {:class "rule__head-predicate-text"
              :on-click #(rf/dispatch [::editor/focus-range
                                       [(get-in rule [:head :predicate :startPosition])
                                        (get-in rule [:head :predicate :endPosition])]])}
       (get-in rule [:head :predicate :text])]
      [:span {:class "rule__do-query"
              :on-click #(rf/dispatch [::el-events/query (get-in rule [:head :repr])])}
       "?"]]
     (if (seq (-> rule :head :args))
       [:<>
        [:div {:class "rule__tutor"} "is true of..."]
        (map-indexed
         (fn [arg-idx arg]
           [:span {:on-click #(rf/dispatch [::editor/focus-range
                                            [(:startPosition arg)
                                             (:endPosition   arg)]])
                   :key arg-idx}
            (let [result-text (get-in query-result [0 :result (inc arg-idx)])]
              (if result-text
                [:span {:class "rule__query-result-arg"
                        :title (get-in query-result [0 :explanation])} result-text]
                (:text arg)))])
         (-> rule :head :args))]
       [:div {:class "rule__tutor"} "is true..."])
     (if (seq (:body rule))
       [:<>
        [:div {:class "rule__tutor"} "when..."]
        (map
         (fn [[src-idx literal]]
           [body-literal {:literal literal
                          :source-index src-idx
                          :key src-idx}])
         (epilog/condense-attributes rule))])]))

(defn rule [{:keys [path] :as props}]
  [views/prerender {:element-type :rule
                    :id path}
   [rule-html props]])

(defn socket-position
  "Find the XY location where a connector should terminate on a particular rule
  and, optionally, body literal."
  [rule-layout literal-id {:keys [end]}]
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

(defn rule-match-connector
  "Draw a line connecting :subgoal (a path to some subgoal) and :rule (a path to a
  rule that matches the subgoal) of `connection`."
  [{:keys [connection]}]
  (let [[sub-rule-head sub-body-idx sub-literal-idx] (:subgoal connection)
        [match-rule-head match-body-idx] (:rule connection)
        subgoal @(rf/subscribe [::el-subs/rule [sub-rule-head sub-body-idx]])
        negative (get-in subgoal [:body sub-literal-idx :negative])
        match-layout @(rf/subscribe [::subs/layout :rule [match-rule-head match-body-idx]])
        sub-layout   @(rf/subscribe [::subs/layout :rule [sub-rule-head sub-body-idx]])
        match-socket (:socket match-layout)
        sub-socket   (get-in sub-layout [:literals sub-literal-idx :socket])
        start (views/center-position sub-socket)
        end   (views/center-position match-socket)]
    [:<>
     [:line {:x1 (:x start)
             :y1 (:y start)
             :x2 (:x end)
             :y2 (:y end)
             :stroke (if negative "red" "black")}]]))

;; This stuff about drawing defeat connectors is obsolete for now, but something
;; similar would be useful for drawing general-purpose connections between rules

(defn defeat-connector
  "Draw a line connecting the defeated and defeater rules from `defeat`."
  [{:keys [defeat]}]
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

(defn defeat-connector-pending
  "Draw a line emanating from the defeated rule while we select another rule to be defeater."
  [{:keys [mouse-position]}]
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
        rule-matches @(rf/subscribe [::el-subs/matches])
        defeatings @(rf/subscribe [::subs/defeatings])
        mouse-position @(rf/subscribe [::subs/mouse-position])]
    [:<>
     #_(map (fn [[id _]]
            [:<> {:key id}
             [fact {:id id
                    :localize-position localize-position}]
             [subobject-connectors {:fact-id id}]])
          (:facts program))
     (mapcat
      (fn [[head bodies]]
        (map-indexed
         (fn [idx _]
           [rule {:path [head idx]
                  :localize-position localize-position
                  :key [head idx]}])
         bodies))
      (:rules program))
     (map (fn [match]
            [rule-match-connector
             {:connection match
              :key (str match)}])
          rule-matches)
     #_(map (fn [defeat]
            [defeat-connector
             {:defeat defeat
              :key (str defeat)}])
          defeatings)
     #_[defeat-connector-pending {:key "defeat-pending"
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
