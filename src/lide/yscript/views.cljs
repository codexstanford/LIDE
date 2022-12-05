(ns lide.yscript.views
  (:require
   [clojure.string :as string]
   [re-frame.core :as rf]
   [lide.editor :as editor]
   [lide.events :as events]
   [lide.subs :as subs]
   [lide.util :as util]
   [lide.views :as views]
   [lide.yscript.core :as ys]
   [lide.yscript.events :as ys-events]
   [lide.yscript.subs :as ys-subs]))

(defn fact-value-element [fact-value]
  (let [value (:value fact-value)
        class (if (= "assertion" (:source fact-value))
                "fact-value fact-value--assertion"
                "fact-value")]
    (cond
      (= value :unknown)
      [:span {:class (str class " fact-value--unknown")} "unknown"]

      (= value true)
      [:span {:class (str class " fact-value--true")} "true"]

      (= value false)
      [:span {:class (str class " fact-value--false")} "false"]

      :else
      [:span {:class class} (str value)])))

(defn next-value [value]
  (case value
    true false
    false :unknown
    :unknown true))

(defn required-fact [{:keys [descriptor range]}]
  (let [fact @(rf/subscribe [::ys-subs/fact descriptor])
        fact-values @(rf/subscribe [::ys-subs/fact-values])]
    [:div {:class "ys-fact"
           :data-fact-descriptor descriptor}
     [:div {:class "ys-fact__descriptor"
            :on-click #(rf/dispatch [::editor/focus-range range])}
      descriptor]
     (let [fact-value (get-in fact-values [descriptor] {:value :unknown})]
       [:div {:class "ys-fact__value"}
        (if (seq (:determiners fact))
          [:<>
           [:div (fact-value-element fact-value)]
           [views/socket]]
          [:div {:on-click #(rf/dispatch [::ys-events/set-fact-value
                                          descriptor
                                          (next-value (:value fact-value))])}
           (fact-value-element fact-value)])])]))

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

(defn expression [{:keys [expr path]}]
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
  (let [program @(rf/subscribe [::subs/program])
        fact-values @(rf/subscribe [::ys-subs/fact-values])
        statement @(rf/subscribe [::ys-subs/statement path])
        local-determinations (ys/compute-statement program fact-values statement)]
    [:div {:class "ys-statement"}
     (case (:type statement)
       "only_if"
       [:div
        (let [{dest-fact-descriptor :descriptor
               dest-fact-range :range} (:dest_fact statement)
              local-determination (get local-determinations dest-fact-descriptor :unknown)]
          [:div {:class "ys-statement__dest-fact"}
           [views/socket]
           [:div {:class "ys-statement__dest-descriptor"
                  :on-click #(rf/dispatch [::editor/focus-range dest-fact-range])}
            dest-fact-descriptor]
           [:div {:class "ys-statement__dest-value"}
            (fact-value-element (get-in fact-values [dest-fact-descriptor] {:value :unknown}))]])
        [:div "ONLY IF"]
        (when (not= :unspecified (:src_expr statement))
          [expression {:expr (:src_expr statement)
                       :path (conj path :src_expr)}])])]))

(defn rule-html [{:keys [name localize-position store-ref]}]
  (let [rule @(rf/subscribe [::ys-subs/rule name])]
    [:div {:class "ys-rule"
           :ref store-ref
           :data-rule-key name
           :on-mouse-down #(rf/dispatch [::events/start-drag (localize-position %) name :rule])}
     [:div {:class "ys-rule__header"}
      "RULE"
      [:span {:class "ys-rule__name"
              :on-click #(rf/dispatch [::editor/focus-range (-> rule :name :range)])}
       name]
      "PROVIDES"]
     (map-indexed
      (fn [idx _]
        [statement {:path [name idx]
                    :key idx}])
      (:statements rule))]))

(defn rule [{:keys [name] :as props}]
  [views/prerender {:element-type :rule
                    :id name}
   [rule-html props]])

(defn statement-socket-position [rule-layout statement-id]
  (merge-with +
              (get-in rule-layout [:container :position])
              (get-in rule-layout [:statements statement-id :position])))

(defn fact-socket-positions
  "Return a set of positions for sockets of `fact-id` in the rule laid out by
  `rule-layout`."
  [rule-layout fact-id]
  (->> (get-in rule-layout [:facts fact-id])
       (map (comp :socket :position))
       (map (fn [socket-position]
              (merge-with +
                          (get-in rule-layout [:container :position])
                          socket-position)))))

(defn requirer-determiner-connectors
  "Draw connectors between `determining-statement-id` and all the instances of
  `fact-id` being required in `requiring-statement-id`."
  [{:keys [descriptor requirer determiner]}]
  (let [[requirer-rule-name   requirer-statement-idx]   requirer
        [determiner-rule-name determiner-statement-idx] determiner
        requirer-rule @(rf/subscribe [::ys-subs/rule requirer-rule-name])
        requirer-layout @(rf/subscribe [::subs/layout :rule requirer-rule-name])
        determiner-rule @(rf/subscribe [::ys-subs/rule determiner-rule-name])
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
  (let [program @(rf/subscribe [::subs/program])
        mouse-position @(rf/subscribe [::subs/mouse-position])]
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
