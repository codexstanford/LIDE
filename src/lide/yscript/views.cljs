(ns lide.yscript.views
  (:require
   [clojure.string :as string]
   [re-frame.core :as rf]
   [lide.events :as events]
   [lide.subs :as subs]
   [lide.views :as views]
   [lide.yscript.subs :as ys-subs]))

;; `expression` and `conjunction-expression` are mutually recursive, so we have
;; to declare `expression` in advance
(declare expression)

(defn conjunction-expression [{:keys [operator exprs]}]
  [:div
   (->> exprs
        (map-indexed (fn [idx expr]
                       [expression {:expr expr
                                    :key idx}]))
        (interpose [:div {:key (random-uuid)}
                    (case operator
                      :and "AND"
                      :or  "OR")]))])

(defn expression [{:keys [expr]}]
  [:div {:class "ys-expr"}
   (cond
     (uuid? expr)
     [:div {:class "ys-fact"
            :data-fact-id expr}
      [:div (:descriptor @(rf/subscribe [::ys-subs/fact expr]))]
      [views/socket]]

     (= :and (:type expr))
     [conjunction-expression {:operator :and
                              :exprs (:exprs expr)}]

     (= :or (:type expr))
     [conjunction-expression {:operator :or
                              :exprs (:exprs expr)}])])

(defn statement [{:keys [id statement]}]
  [:div {:class "ys-statement"
         :data-statement-id id}
   (case (:type statement)
     :only-if
     [:div
      [:div
       [views/socket]
       [:div (:descriptor @(rf/subscribe [::ys-subs/fact (:dest-fact statement)]))]]
      [:div "ONLY IF"]
      [expression {:expr (:src-expr statement)}]])])

(defn rule-html [{:keys [id localize-position store-ref]}]
  (let [rule @(rf/subscribe [::ys-subs/rule id])]
    [:div {:class "ys-rule"
           :ref store-ref
           :data-rule-id id
           :on-mouse-down #(rf/dispatch [::events/start-drag (localize-position %) id])}
     [:div {:class "ys-rule__header"}
      [:div {:class "ys-rule__name"} (if-not (string/blank? (:name rule))
                                       (:name rule)
                                       "[unnamed rule]")]]
     (->> (:statements rule)
          (map (fn [[id st]]
                 [statement {:id id
                             :statement st
                             :key id}])))]))

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
  `required-fact-id` being required in `rule-id`."
  [{:keys [fact-id
           requiring-rule-id
           determining-statement-id]}]
  (let [requirer-layout @(rf/subscribe [::subs/layout :ys-rule requiring-rule-id])
        determiner-rule @(rf/subscribe [::ys-subs/rule-containing-statement
                                        determining-statement-id])
        determiner-layout @(rf/subscribe [::subs/layout :ys-rule determiner-rule])]
    [:<>
     (->> (get-in requirer-layout [:facts fact-id])
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
  (let [requiring-rules @(rf/subscribe [::ys-subs/rules-requiring-fact fact-id])
        determining-statements @(rf/subscribe [::ys-subs/statements-determining-fact fact-id])]
    [:<>
     (->> requiring-rules
          (mapcat
           (fn [requirer]
             (->> determining-statements
                  (map
                   (fn [determiner]
                     [requirer-determiner-connectors {:fact-id fact-id
                                                      :requiring-rule-id requirer
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
