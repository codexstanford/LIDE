(ns lide.yscript.views
  (:require
   [clojure.string :as string]
   [re-frame.core :as rf]
   [lide.events :as events]
   [lide.subs :as subs]
   [lide.util :as util]
   [lide.views :as views]
   [lide.yscript.core :as ys]
   [lide.yscript.events :as ys-events]
   [lide.yscript.subs :as ys-subs]))

(defn fact-value [fact]
  (if (= :unknown (:value fact))
    "unknown"
    (str (:value fact))))

(defn next-value [value]
  (case value
    true false
    false :unknown
    :unknown true))

(defn required-fact [{:keys [expr path]}]
  (let [fact-values @(rf/subscribe [::ys-subs/fact-values])
        determiners @(rf/subscribe [::ys-subs/statements-determining-fact (:descriptor expr)])]
    [:div {:class "ys-fact"
           :data-fact-id (:descriptor expr)}
     [util/eip-plain-text
      {:value (:descriptor expr)
       :on-blur #(rf/dispatch [::ys-events/set-requiree-descriptor
                               path
                               (-> % .-target .-value)])
       :placeholder "[not set]"}]
     (let [fact-value (get fact-values (:descriptor expr) :unknown)]
       [:div {:class "ys-fact__value"}
        (if (seq determiners)
          [:<>
           [:div fact-value]
           [views/socket]]
          [:div {:on-click #(rf/dispatch [::ys-events/set-fact-value
                                          (:descriptor expr)
                                          (next-value fact-value)])}
           fact-value])])]))

(defn flatten-conjunctions [expr]
  ;; TODO this doesn't tolerate manual association via BEGIN/END
  (if (= "fact_expr" (:type expr))
    [{:descriptor (:descriptor expr)
      :operator ""
      :range (:range expr)}]

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
        [:div {:on-click #(rf/dispatch [::ys-events/select-range (:range expr-line)])
               :key idx}
         (str (:descriptor expr-line)
              " "
              (:operator expr-line))])
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
        (let [local-determination (get local-determinations (:dest_fact statement))]
          [:div {:class "ys-statement__dest-fact"}
           [views/socket]
           [util/eip-plain-text
            {:value (:dest_fact statement)
             :on-blur #(rf/dispatch [::ys-events/set-dest-fact
                                     path
                                     (-> % .-target .-value)])
             :placeholder "[not set]"}]
           [:div {:class "ys-statement__dest-value"}
            ;; Warn about stale determinations if there is a value in the app DB
            ;; for dest_fact, and the statement has a local determination for
            ;; it, but the values differ
            (let [global-value (get-in fact-values [(:dest_fact statement) :value])]
              (when (and global-value
                         local-determination
                         (not= global-value local-determination))
                [:div {:class "ys-statement__warn-stale"} "(stale)"]))
            (fact-value local-determination)]])
        [:div "ONLY IF"]
        (if (= :unspecified (:src_expr statement))
          [:select {:on-change #(do
                                  (when (-> % .-target .-value)
                                    (rf/dispatch [::ys-events/add-source-expr
                                                  path
                                                  (keyword (-> % .-target .-value))]))
                                  (set! (-> % .-target .-value) ""))}
           [:option {:value ""} "Add expression..."]
           [:option {:value "and"} "AND"]
           [:option {:value "or"} "OR"]]
          [expression {:expr (:src_expr statement)
                       :path (conj path :src_expr)}])])]))

(defn rule-html [{:keys [name localize-position store-ref]}]
  (let [rule @(rf/subscribe [::ys-subs/rule name])]
    [:div {:class "ys-rule"
           :ref store-ref
           :data-rule-key name
           :on-mouse-down #(rf/dispatch [::events/start-drag (localize-position %) name :rule])
           :on-click #(rf/dispatch [::ys-events/show-range (:range rule)])}
     [:div {:class "ys-rule__header"}
      "RULE "
      [util/eip-plain-text
       {:value name
        ;; TODO should validate (esp against empty)
        :on-blur #(rf/dispatch [::ys-events/set-rule-name name (-> % .-target .-value)])
        :class "ys-rule__name"}]
      " PROVIDES"]
     (map-indexed
      (fn [idx _]
        [statement {:path [name idx]
                    :key idx}])
      (:statements rule))
     [:select {:on-change #(do
                             (when (-> % .-target .-value)
                               (rf/dispatch [::ys-events/add-statement name (keyword (-> % .-target .-value))]))
                             (set! (-> % .-target .-value) ""))}
      [:option {:value ""} "Add statement..."]
      [:option {:value "only-if"} "ONLY IF"]]]))

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
  [{:keys [fact-id
           requiring-statement-id
           determining-statement-id]}]
  (let [requirer-statement @(rf/subscribe [::ys-subs/statement requiring-statement-id])
        requirer-rule @(rf/subscribe [::ys-subs/rule-containing-statement requiring-statement-id])
        requirer-layout @(rf/subscribe [::subs/layout :ys-rule requirer-rule])
        determiner-rule @(rf/subscribe [::ys-subs/rule-containing-statement
                                        determining-statement-id])
        determiner-layout @(rf/subscribe [::subs/layout :ys-rule determiner-rule])]
    [:text "hello"]
    [:<>
     (->> (get-in requirer-layout [:statements requiring-statement-id :facts fact-id])
          (map-indexed
           (fn [idx fact-layout]
             (let [determiner-socket
                   (merge-with +
                               (get-in determiner-layout [:container :position])
                               (views/center-position
                                (get-in determiner-layout [:statements
                                                           determining-statement-id
                                                           :socket])))
                   requiring-socket
                   (merge-with +
                               (get-in requirer-layout [:container :position])
                               (views/center-position
                                (get-in fact-layout [:socket])))]
               [:line {:x1 (:x requiring-socket)
                       :y1 (:y requiring-socket)
                       :x2 (:x determiner-socket)
                       :y2 (:y determiner-socket)
                       :stroke "black"
                       :key idx}]))))]))

(defn fact-determiner-connectors
  "Draw connectors between all the instances of `fact-id` being required and the
  instances of it being determined."
  [{:keys [fact-id]}]
  (let [requiring-statements @(rf/subscribe [::ys-subs/statements-requiring-fact fact-id])
        determining-statements @(rf/subscribe [::ys-subs/statements-determining-fact fact-id])]
    [:<>
     (->> requiring-statements
          (mapcat
           (fn [requirer]
             (->> determining-statements
                  (map
                   (fn [determiner]
                     [requirer-determiner-connectors {:fact-id fact-id
                                                      :requiring-statement-id requirer
                                                      :determining-statement-id determiner
                                                      :key (str fact-id requirer determiner)}]))))))]))

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
           (fn [[fact-id _]]
             [fact-determiner-connectors {:fact-id fact-id
                                          :key fact-id}])))]))

(defn code-panel []
  (let [program @(rf/subscribe [::ys-subs/populated-program])
        rule-order @(rf/subscribe [::ys-subs/rule-order])]
    [:div {:class "yscript-inspector"}
     [:textarea {:class "code"
                 :defaultValue (ys/codify-program program rule-order)
                 :on-blur #(rf/dispatch [::ys-events/code-updated (-> % .-target .-value)])}]]))
