(ns lide.yscript.views
  (:require
   [clojure.string :as string]
   [re-frame.core :as rf]
   [lide.events :as events]
   [lide.subs :as subs]
   [lide.util :as util]
   [lide.views :as views]
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

(defn required-fact [{:keys [fact-id path]}]
  (let [fact @(rf/subscribe [::ys-subs/fact fact-id])
        determiners @(rf/subscribe [::ys-subs/statements-determining-fact fact-id])]
    [:div {:class "ys-fact"
           :data-fact-id fact-id}
     [util/eip-plain-text
      {:value (:descriptor fact)
       :on-blur #(rf/dispatch [::ys-events/set-requiree-descriptor
                               path
                               (-> % .-target .-value)])
       :placeholder "[not set]"}]
     [:div {:class "ys-fact__value"}
      (if (seq determiners)
        [:<>
         [:div (fact-value fact)]
         [views/socket]]
        [:div {:on-click #(rf/dispatch [::ys-events/set-fact-value fact-id (next-value (:value fact))])}
         (fact-value fact)])]]))

;; `expression` and `conjunction-expression` are mutually recursive, so we have
;; to declare `expression` in advance
(declare expression)

(defn conjunction-expression [{:keys [operator exprs path]}]
  [:div
   (->> exprs
        (map-indexed (fn [idx expr]
                       [expression {:expr expr
                                    :path (conj path idx)
                                    :key idx}]))
        (interpose [:div {:key (random-uuid)}
                    (case operator
                      :and "AND"
                      :or  "OR")]))])

(defn expression [{:keys [expr path]}]
  [:div {:class "ys-expr"}
   (cond
     (uuid? expr)
     [required-fact {:fact-id expr
                     :path path}]

     (= :and (:type expr))
     [conjunction-expression {:operator :and
                              :exprs (:exprs expr)
                              :path (conj path :exprs)}]

     (= :or (:type expr))
     [conjunction-expression {:operator :or
                              :exprs (:exprs expr)
                              :path (conj path :exprs)}])])

(defn statement [{:keys [id]}]
  (let [statement @(rf/subscribe [::ys-subs/statement id])]
    [:div {:class "ys-statement"
           :data-statement-id id}
     (case (:type statement)
       :only-if
       [:div
        (let [fact @(rf/subscribe [::ys-subs/fact (:dest-fact statement)])]
          [:div {:class "ys-statement__dest-fact"}
           [views/socket]
           [util/eip-plain-text
            {:value (:descriptor fact)
             :on-blur #(rf/dispatch [::ys-events/set-determinee-descriptor
                                     id
                                     (-> % .-target .-value)])
             :placeholder "[not set]"}]
           [:div {:class "ys-statement__dest-value"} (fact-value fact)]])
        [:div "ONLY IF"]
        (if (= :unspecified (:src-expr statement))
          [:select {:on-change #(do
                                  (when (-> % .-target .-value)
                                    (rf/dispatch [::ys-events/add-source-expr
                                                  id
                                                  (keyword (-> % .-target .-value))]))
                                  (set! (-> % .-target .-value) ""))}
           [:option {:value ""} "Add expression..."]
           [:option {:value "and"} "AND"]
           [:option {:value "or"} "OR"]]
          [expression {:expr (:src-expr statement)
                       :path [id :src-expr]}])])]))

(defn rule-html [{:keys [id localize-position store-ref]}]
  (let [rule @(rf/subscribe [::ys-subs/rule id])]
    [:div {:class "ys-rule"
           :ref store-ref
           :data-rule-id id
           :on-mouse-down #(rf/dispatch [::events/start-drag (localize-position %) id])}
     [:div {:class "ys-rule__header"}
      [:div {:class "ys-rule__name"} (str "RULE "
                                          (if-not (string/blank? (:name rule))
                                            (:name rule)
                                            "[not named]")
                                          " PROVIDES")]]
     (->> (:statements rule)
          (map (fn [st-id]
                 [statement {:id st-id
                             :key st-id}])))
     [:select {:on-change #(do
                             (when (-> % .-target .-value)
                               (rf/dispatch [::ys-events/add-statement id (keyword (-> % .-target .-value))]))
                             (set! (-> % .-target .-value) ""))}
      [:option {:value ""} "Add statement..."]
      [:option {:value "only-if"} "ONLY IF"]]]))

(defn rule [{:keys [id] :as props}]
  [views/prerender {:element-type :ys-rule
                    :id id}
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
          (map (fn [[id _]]
                 [rule {:id  id
                        :localize-position localize-position
                        :key    id}])))
     (->> (:facts program)
          (map
           (fn [[fact-id _]]
             [fact-determiner-connectors {:fact-id fact-id
                                          :key fact-id}])))]))
