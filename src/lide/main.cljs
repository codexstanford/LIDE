(ns lide.main
  (:require
   [lide.epilog.views :as epilog-views]
   [lide.events :as events]
   [lide.subs :as subs]
   [lide.util :as util]
   [lide.views :as views]
   [lide.yscript.views :as ys-views]
   [re-frame.core :as rf]))

(defn graph-viewport
  "Draw an SVG group to contain the program graph.

  Using a group is useful because we can apply scale/translation
  transformations to the entire graph at once."
  [{:keys [set-ref]} & children]
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
            target @(rf/subscribe [::subs/program-target])]
        (when program
          [:svg {:class "graph-panel"
                 :id "graph-svg"
                 ;; TODO throttle these events
                 :on-mouse-move #(rf/dispatch [::events/mouse-move % (localize-position %)])
                 :on-mouse-up #(rf/dispatch [::events/mouse-up (localize-position %)])
                 :on-wheel #(rf/dispatch [::events/scroll-graph %])}
           [:rect {:class "graph__bg"
                   :height 10000
                   :width  10000
                   :on-mouse-down #(rf/dispatch [::events/mouse-down-graph-bg %])}]
           [graph-viewport
            {:set-ref #(reset! !svg-viewport %)}
            (let [graph-props {:localize-position localize-position
                               :key :program-graph}]
              (case target
                :yscript [ys-views/program-graph graph-props]
                [epilog-views/program-graph graph-props]))]])))))

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
  (let [show-toolbar? @(rf/subscribe [::subs/show-toolbar?])]
    [:div {:id "app-container"}
     [:div {:class "work-viewport"}
      [program-graph]]
     (when show-toolbar?
       [toolbar])]))
