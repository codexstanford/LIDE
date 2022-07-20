(ns lide.core
  (:require
   [reagent.dom :as rdom]
   [re-frame.core :as rf]
   [lide.config :as config]
   [lide.events :as events]
   [lide.main :as main]
   [lide.util :as util]
   [lide.yscript.events :as ys-events]))

(defn dev-setup []
  (when config/debug?
    (println "dev mode")))

(defn ^:dev/after-load mount-root []
  (rf/clear-subscription-cache!)
  (let [root-el (.getElementById js/document "app")]
    (rdom/unmount-component-at-node root-el)
    (rdom/render [main/main-panel] root-el)))

(defn parse-positions [^js positions]
  (->> (js->clj positions)
       (util/map-vals (fn [position]
                        {:x (get position "x")
                         :y (get position "y")}))))

(defn init []
  (dev-setup)
  (mount-root)
  (rf/dispatch-sync [::events/initialize-db])
  (when (.-acquireVsCodeApi js/window)
    (rf/dispatch-sync [::events/vs-code-api (. js/window acquireVsCodeApi)]))
  ;; Global listeners for Ctrl+Z, VS Code messages, ...
  (.addEventListener js/window
                     "message"
                     (fn [^js message]
                       (cond
                         (= "yscript.graph.codeUpdated" (-> message .-data .-type))
                         (rf/dispatch [::ys-events/code-updated (-> message .-data .-text)])

                         (= "yscript.graph.positionsRead" (-> message .-data .-type))
                         (rf/dispatch [::ys-events/positions-read
                                       (parse-positions (-> message .-data .-positions))]))))
  (.addEventListener js/document
                     "keydown"
                     (fn [event]
                       (cond
                         (= "Escape" (.-key event))
                         (rf/dispatch [::events/escape])

                         (and (.-ctrlKey event)
                              (= "z" (.-key event)))
                         (rf/dispatch [:undo])))))
