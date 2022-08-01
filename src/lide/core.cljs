(ns lide.core
  (:require
   [reagent.dom :as rdom]
   [re-frame.core :as rf]
   [lide.config :as config]
   [lide.events :as events]
   [lide.main :as main]
   [lide.util :as util]
   [lide.epilog.events :as epilog-events]
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
  (let [message (-> event .-data)]
    (case (.-type message)
      "lide.initForLanguage"
      (rf/dispatch [::events/initialize-db (keyword (.-language message))])

      "lide.codeUpdated.epilog"
      (rf/dispatch [::epilog-events/code-updated (.-model message)])

      "lide.codeUpdated.yscript"
      (rf/dispatch [::ys-events/code-updated (.-model message)])

      "lide.positionsRead"
      (rf/dispatch [::events/positions-read
                    (parse-positions (.-positions message))]))))

(defn init []
  (dev-setup)
  (mount-root)
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
                         (rf/dispatch [:undo]))))
  (rf/dispatch-sync [::events/vs-code-api (. js/window acquireVsCodeApi)])
  (rf/dispatch-sync [::events/app-ready]))
