(ns lide.views
  (:require
   [lide.events :as events]
   [lide.subs :as subs]
   [re-frame.core :as rf]
   [reagent.core :as r]))

(defn center-position
  "Find the center point of a square at `position` with `size`."
  [{:keys [position size]}]
  {:x (+ (:x position) (/ (:width size) 2))
   :y (+ (:y position) (/ (:height size) 2))})

(defn localize-event-to-svg
  "Find the coordinates of `event` in `svg`'s local coordinate space."
  [^js svg event]
  (let [dom-point (js/DOMPoint. (.-clientX event) (.-clientY event))
        svg-point (.matrixTransform dom-point (.inverse (.getScreenCTM svg)))]
    {:x (.-x svg-point)
     :y (.-y svg-point)}))

(defn normalize-hiccup
  "Make sure Hiccup vector `hic` contains an attribute map."
  [hic]
  (cond
    (= 1 (count hic)) (conj hic {})
    (not (map? (nth hic 1))) (vec (concat [(first hic)] [{}] (rest hic)))
    :else hic))

(defn prerender [{:keys [element-type id]} wrapped-component]
  (let [element (r/atom nil)
        generation (r/atom 0)
        on-render #(do
                     (swap! generation inc)
                     (rf/dispatch [::events/rendered element-type id @element @generation]))]
    (r/create-class
     {:display-name (name element-type)

      :component-did-mount  on-render
      :component-did-update on-render

      :reagent-render
      (fn []
        (let [position @(rf/subscribe [::subs/position element-type id])]
          [:foreignObject {:width 1
                           :height 1
                           :style {"overflow" "visible"}
                           :transform (str "translate(" (:x position) ", " (:y position) ")")}
           [:div {:class "prerender-wrapper"}
            (let [[tag attrs & grandchildren] (normalize-hiccup wrapped-component)]
              (vec
               (concat
                [tag (merge attrs {:store-ref #(reset! element %)})]
                grandchildren)))]]))})))

(defn socket [props]
  [:div (merge props {:class "socket"})])
