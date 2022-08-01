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
  (-> (js->clj positions)
      (clojure.set/rename-keys {"rule" :rule})
      (update
       :rule
       (fn [rule-positions]
         (util/map-vals
          (fn [position]
            {:x (get position "x")
             :y (get position "y")})
          rule-positions)))))

(defn handle-message [^js event]
  (let [message-data (-> event .-message .-data)]
    (case (.-type message-data)
      "codeUpdated.yscript"
      (rf/dispatch [::ys-events/code-updated (.-model message-data)])

      "positionsRead"
      (rf/dispatch [::events/positions-read
                    (parse-positions (.-positions message-data))]))))

(defn init []
  (dev-setup)
  (mount-root)
  (rf/dispatch-sync [::events/initialize-db])
  (when (.-acquireVsCodeApi js/window)
    (rf/dispatch-sync [::events/vs-code-api (. js/window acquireVsCodeApi)]))
  ;; Global listeners for Ctrl+Z, VS Code messages, ...
  (.addEventListener js/window "message" handle-message)
  (.addEventListener js/document
                     "keydown"
                     (fn [event]
                       (cond
                         (= "Escape" (.-key event))
                         (rf/dispatch [::events/escape])

                         (and (.-ctrlKey event)
                              (= "z" (.-key event)))
                         (rf/dispatch [:undo])))))
