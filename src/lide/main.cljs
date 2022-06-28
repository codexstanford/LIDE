(ns lide.main
  (:require
   [clojure.string :as string]
   [re-frame.core :as rf]
   [lide.events :as events]
   [lide.subs :as subs]
   [lide.util :as util]
   [lide.views :as views]
   [lide.epilog.core :as epilog]
   [lide.epilog.views :as epilog-views]))

(defn graph-viewport [{:keys [set-ref]} & children]
  "Draw an SVG group to contain the program graph.

  Using a group is useful because we can apply scale/translation
  transformations to the entire graph at once."
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
                 :on-mouse-move (goog.functions.throttle
                                 #(rf/dispatch [::events/mouse-move (localize-position %)])
                                 25)
                 :on-mouse-up #(rf/dispatch [::events/mouse-up (localize-position %)])
                 :on-wheel (goog.functions.throttle #(rf/dispatch [::events/scroll-graph %])
                                                    100)}
           [:rect {:class "graph__bg"
                   :height 10000
                   :width  10000
                   :on-mouse-down #(rf/dispatch [::events/mouse-down-graph-bg (localize-position %)])}]
           [graph-viewport
            {:set-ref #(reset! !svg-viewport %)}
            [epilog-views/program-graph {:localize-position localize-position
                                         :key :program-graph}]]])))))

(defn epilog-panel []
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
  [:div {:id "app-container"}
   [:div {:class "work-viewport"}
    [program-graph]
    [:div {:class "inspectors"}
     [epilog-panel]]]
   [toolbar]])
