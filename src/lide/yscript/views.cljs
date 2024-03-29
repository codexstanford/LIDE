(ns lide.yscript.views
  (:require
   [clojure.string :as string]
   [lide.editor :as editor]
   [lide.events :as events]
   [lide.subs :as subs]
   [lide.views :as views]
   [lide.yscript.events :as ys-events]
   [lide.yscript.subs :as ys-subs]
   [re-frame.core :as rf]))

(defn fact-controls [descriptor fact-value]
  (let [value (:value fact-value)
        class (if (= "assertion" (:source fact-value))
                "ys-rule-fact__value ys-rule-fact__value--assertion"
                "ys-rule-fact__value")
        handle-click #(rf/dispatch [::ys-events/set-fact-value
                                    descriptor
                                    (cond
                                      (= "assertion" (:source fact-value))
                                      (not (:value fact-value))

                                      (not= :unknown (:value fact-value))
                                      (:value fact-value)

                                      :else
                                      true)])]
    [:div {:class "ys-rule-fact__controls"}
     (when (= "assertion" (:source fact-value))
       [:div {:class "ys-rule-fact__unassert"
              :on-click #(rf/dispatch [::ys-events/set-fact-value
                                       descriptor
                                       :unknown])}
        "🗙"])
     (cond
       (= value :unknown)
       [:div {:class (str class " ys-rule-fact__value--unknown")
              :on-click handle-click}
        "unknown"]

       (= value true)
       [:div {:class (str class " ys-rule-fact__value--true")
              :on-click handle-click}
        "true"]

       (= value false)
       [:div {:class (str class " ys-rule-fact__value--false")
              :on-click handle-click}
        "false"]

       :else
       [:div {:class class} (str value)])]))

(defn required-fact [{:keys [descriptor range]}]
  (let [fact @(rf/subscribe [::ys-subs/fact descriptor])
        goal? (= descriptor @(rf/subscribe [::ys-subs/next-for-goal]))
        fact-values @(rf/subscribe [::ys-subs/fact-values])]
    [:div {:class "ys-rule-fact"
           :data-fact-descriptor descriptor}
     [:div {:class "ys-rule-fact__descriptor"
            :on-click #(rf/dispatch [::editor/focus-range range])}
      descriptor]
     (let [fact-value (get-in fact-values [descriptor] {:value :unknown})]
       [:<>
        (when goal? [:div {:class "ys-next-marker"}])
        (fact-controls descriptor fact-value)
        (when (seq (:determiners fact))
          [views/socket])])]))

(defn flatten-conjunctions [expr]
  ;; TODO this doesn't tolerate manual association via BEGIN/END
  (cond
    (= (:type expr) "fact_expr")
    [{:descriptor (:descriptor expr)
      :operator ""
      :range (:range expr)}]

    ;; Note this doesn't support nested NOTs, which you shouldn't do anyway
    (= (:type expr) "not_expr")
    [{:operator "NOT"
      :range (:range expr)}
     {:descriptor (-> expr :negand :descriptor)
      :range (-> expr :negand :range)}]

    :else
    (let [operator (case (:type expr)
                     "and_expr" "AND"
                     "or_expr"  "OR")
          left (flatten-conjunctions (:left expr))
          left-and (update left (dec (count left)) #(assoc % :operator operator))]
      (vec
       (concat left-and
               (flatten-conjunctions (:right expr)))))))

(defn expression [{:keys [expr]}]
  [:div {:class "ys-expr"}
   (let [lines (flatten-conjunctions expr)]
     (map-indexed
      (fn [idx expr-line]
        [:<> {:key idx}
         (when (not (string/blank? (:descriptor expr-line)))
           [required-fact expr-line])
         (when (not (string/blank? (:operator expr-line)))
           [:div (:operator expr-line)])])
      lines))])

(defn statement [{:keys [path]}]
  (let [fact-values @(rf/subscribe [::ys-subs/fact-values])
        statement @(rf/subscribe [::ys-subs/statement path])]
    [:div {:class "ys-statement"}
     (let [{dest-fact-descriptor :descriptor
            dest-fact-range :range} (:dest_fact statement)]
       (case (:type statement)
         "if_then"
         [:<>
          [:div "IF"]
          (when (not= :unspecified (:src_expr statement))
            [expression {:expr (:src_expr statement)
                         :path (conj path :src_expr)}])
          [:div "THEN"]
          [:div {:class "ys-rule-fact ys-dest-fact"}
           [views/socket]
           [:div {:class "ys-rule-fact__descriptor"
                  :on-click #(rf/dispatch [::editor/focus-range dest-fact-range])}
            dest-fact-descriptor]
           (fact-controls dest-fact-descriptor
                          (get-in fact-values [dest-fact-descriptor] {:value :unknown}))]]

         "only_if"
         [:<>
          [:div {:class "ys-rule-fact ys-dest-fact"}
           [views/socket]
           [:div {:class "ys-rule-fact__descriptor"
                  :on-click #(rf/dispatch [::editor/focus-range dest-fact-range])}
            dest-fact-descriptor]
           (fact-controls dest-fact-descriptor
                          (get-in fact-values [dest-fact-descriptor] {:value :unknown}))]
          [:div "ONLY IF"]
          (when (not= :unspecified (:src_expr statement))
            [expression {:expr (:src_expr statement)
                         :path (conj path :src_expr)}])]))]))

(defn rule-html [{:keys [name localize-position store-ref]}]
  (let [rule @(rf/subscribe [::ys-subs/rule name])
        goal? (= name @(rf/subscribe [::ys-subs/goal-rule]))]
    [:div {:class "ys-rule"
           :ref store-ref
           :data-rule-key name
           :on-mouse-down #(rf/dispatch [::events/start-drag (localize-position %) name :rule])}
     [:div {:class "ys-rule__header"}
      (when goal? [:span {:class "ys-rule__goal"} "GOAL"])
      "RULE"
      [:span {:class "ys-rule__name"
              :on-click #(rf/dispatch [::editor/focus-range (-> rule :name :range)])}
       name]
      "PROVIDES"
      [:span {:class (str "ys-rule__set-goal"
                          (when goal? " ys-rule__set-goal--goal"))
              :on-click #(rf/dispatch [::ys-events/set-goal (if goal? nil name)])}]]
     (map-indexed
      (fn [idx _]
        [statement {:path [name idx]
                    :key idx}])
      (:statements rule))]))

(defn rule [{:keys [name] :as props}]
  [views/prerender {:element-type :rule
                    :id name}
   [rule-html props]])

(defn requirer-determiner-connectors
  "Draw connectors between `determining-statement-id` and all the instances of
  `fact-id` being required in `requiring-statement-id`."
  [{:keys [descriptor requirer determiner]}]
  (let [[requirer-rule-name   requirer-statement-idx]   requirer
        [determiner-rule-name determiner-statement-idx] determiner
        requirer-layout @(rf/subscribe [::subs/layout :rule requirer-rule-name])
        determiner-layout @(rf/subscribe [::subs/layout :rule determiner-rule-name])]
    [:<>
     (->> (get-in requirer-layout [:statements requirer-statement-idx :facts descriptor])
          (map-indexed
           (fn [idx fact-layout]
             (let [determiner-socket
                   (merge-with +
                               (get-in determiner-layout [:container :position])
                               (views/center-position
                                (get-in determiner-layout [:statements
                                                           determiner-statement-idx
                                                           :socket])))
                   requirer-socket
                   (merge-with +
                               (get-in requirer-layout [:container :position])
                               (views/center-position
                                (get-in fact-layout [:socket])))]
               [:line {:x1 (:x requirer-socket)
                       :y1 (:y requirer-socket)
                       :x2 (:x determiner-socket)
                       :y2 (:y determiner-socket)
                       :stroke "black"
                       :key idx}]))))]))

(defn fact-determiner-connectors
  "Draw connectors between all the instances of fact with `descriptor` being
  required and the instances of it being determined."
  [{:keys [descriptor]}]
  (let [fact @(rf/subscribe [::ys-subs/fact descriptor])]
    [:<>
     (->> (:requirers fact)
          (mapcat
           (fn [requirer]
             (->> (:determiners fact)
                  (map
                   (fn [determiner]
                     [requirer-determiner-connectors {:descriptor descriptor
                                                      :requirer (:path requirer)
                                                      :determiner (:path determiner)
                                                      :key (str descriptor requirer determiner)}]))))))]))

(defn program-graph [{:keys [localize-position]}]
  (let [program @(rf/subscribe [::subs/program])]
    [:<>
     (->> (:rules program)
          (map (fn [[name _]]
                 [rule {:name name
                        :localize-position localize-position
                        :key name}])))
     (->> (:facts program)
          (map
           (fn [[descriptor _]]
             [fact-determiner-connectors {:descriptor descriptor
                                          :key descriptor}])))]))
