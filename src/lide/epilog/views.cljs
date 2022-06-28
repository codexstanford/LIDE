(ns lide.epilog.views
  (:require
   [re-frame.core :as rf]
   [lide.events :as events]
   [lide.subs :as subs]
   [lide.util :as util]
   [lide.views :as views]))

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

(defn body-literal [{:keys [id]}]
  (let [literal @(rf/subscribe [::subs/literal id])]
    [:div {:class "body-literal"
           :data-literal-id id}
     [:div {:class "body-literal__predicate"}
      [util/eip-plain-text
       {:value (:predicate literal)
        :on-blur #(rf/dispatch [::events/edit-literal-predicate id (-> % .-target .-value)])}]
      [:button {:title "Negate"
                :class "rule__button"
                :on-click #(rf/dispatch [::events/negate-literal id])}
       "not"]
      [views/socket]]
     (if (seq (:args literal))
       [:<>
        [:div {:class "rule__tutor"} "is " (when (:negative literal) [:span {:class "rule__tutor--stress"} "not "]) "true of..."]
        (map-indexed
         (fn [arg-index arg]
           [util/eip-plain-text
            {:value arg
             :on-blur #(rf/dispatch [::events/edit-literal-arg id arg-index (-> % .-target .-value)])
             :style util/style-arg
             :key arg}])
         (:args literal))
        [:div {:class "button-add"
               :on-click #(rf/dispatch [::events/add-literal-argument id])}
         "+ and..."]]

       [:div {:class "button-add"
              :on-click #(rf/dispatch [::events/add-literal-argument id])}
        "+ is true of..."])]))

(defn fact [{:keys [id localize-position]}]
  (let [fact @(rf/subscribe [::subs/fact id])
        position @(rf/subscribe [::subs/position :fact id])]
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
  (let [rule @(rf/subscribe [::subs/populated-rule id])]
    [:div {:class "rule"
           :ref store-ref
           :on-mouse-down #(rf/dispatch [::events/start-drag-rule (localize-position %) id])}
     [:div {:class "rule__head-predicate"}
      [views/socket {:on-click #(rf/dispatch [::events/select-defeater id])}]
      [util/eip-plain-text
       {:value (-> rule :head :predicate)
        :on-blur #(rf/dispatch [::events/edit-head-predicate id (-> % .-target .-value)])}]]
     (if (seq (-> rule :head :args))
       [:<>
        [:div {:class "rule__tutor"} "is true of..."]
        [:<>
         (map-indexed
          (fn [arg-index arg]
            [util/eip-plain-text
             {:value arg
              :on-blur #(rf/dispatch [::events/edit-head-arg id arg-index (-> % .-target .-value)])
              :style util/style-arg
              :key arg-index}])
          (-> rule :head :args))]
        [:div {:class "rule__add-arg button-add"
               :on-click #(rf/dispatch [::events/add-argument id])}
         "+ and..."]]
       [:div {:class "rule__add-arg button-add"
              :on-click #(rf/dispatch [::events/add-argument id])}
        "+ is true of..."])
     (if (seq (:body rule))
       [:<>
        [:div {:class "rule__tutor"} "when..."]
        [:<>
         (map
          (fn [literal]
            [body-literal {:id (:id literal)
                           :key (:id literal)}])
          (:body rule))]
        [:div {:class "rule__add-arg button-add"
               :on-click #(rf/dispatch [::events/add-body-literal id])}
         "+ and..."]]
       [:div {:class "rule__add-arg button-add"
              :on-click #(rf/dispatch [::events/add-body-literal id])}
        "+ when..."])
     [:div {:class "rule__add-defeater button-add"
            :on-click #(rf/dispatch [::events/defeated-selecting-defeater id])}
      [:div {:class "rule__add-defeater-label"} "+ unless..."]
      [views/socket]]]))

(defn rule [{:keys [id] :as props}]
  [views/prerender {:element-type :rule
                    :id id}
   [rule-html props]])
