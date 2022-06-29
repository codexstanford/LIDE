(ns lide.yscript.views
  (:require
   [clojure.string :as string]
   [re-frame.core :as rf]
   [lide.events :as events]
   [lide.subs :as subs]
   [lide.views :as views]
   [lide.yscript.subs :as ys-subs]))

(defn fact [{:keys [id] :as props}]
  [views/prerender {:element-type :fact
                    :id id}
   [fact-html props]])

;; `expression` and `conjunction-expression` are mutually recursive, so we have
;; to declare `expression` in advance
(declare expression)

(defn conjunction-expression [{:keys [operator exprs]}]
  [:div
   (->> exprs
        (map-indexed (fn [i expr]
                       [expression {:expr %
                                    :key i}]))
        (interpose [:div {:key (random-uuid)}
                    (case operator
                      :and "AND"
                      :or  "OR")]))])

(defn expression [{:keys [expr]}]
  [:div {:class "ys-expr"}
   (cond
     (uuid? expr)
     [:div {:class "ys-fact"}
      [:div (:descriptor @(rf/subscribe [::ys-subs/fact expr]))]
      [views/socket]]

     (= :and (:type expr))
     [conjunction-expression {:operator :and
                              :exprs (:exprs expr)}]

     (= :or (:type expr))
     [conjunction-expression {:operator :or
                              :exprs (:exprs expr)}])])

(defn statement [{:keys [statement]}]
  [:div {:class "ys-statement"}
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
           :on-mouse-down #(rf/dispatch [::events/start-drag (localize-position %) id])}
     [:div {:class "ys-rule__header"}
      [:div {:class "ys-rule__name"} (if-not (string/blank? (:name rule))
                                       (:name rule)
                                       "[unnamed rule]")]]
     (->> (:statements rule)
          (map (fn [[id st]]
                 [statement {:statement st
                             :key id}])))]))

(defn rule [{:keys [id] :as props}]
  [views/prerender {:element-type :rule
                    :id id}
   [rule-html props]])

(defn fact-match-connector []
  )

(defn program-graph [{:keys [localize-position]}]
  (let [program @(rf/subscribe [::subs/program])
        fact-matches @(rf/subscribe [::ys-subs/fact-matches])
        mouse-position @(rf/subscribe [::subs/mouse-position])]
    [:<>
     (->> (:rules program)
          (map (fn [[id _]]
                 [rule {:id  id
                        :localize-position localize-position
                        :key    id}])))
     (->> fact-matches
          (map (fn [match]
                 [fact-match-connector
                  {:connection match
                   :key (str match)}])))]))
