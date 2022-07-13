(ns lide.core
  (:require
   [reagent.dom :as rdom]
   [re-frame.core :as rf]
   [lide.events :as events]
   [lide.main :as main]
   [lide.config :as config]
   [lide.yscript.events :as ys-events]))

(defn dev-setup []
  (when config/debug?
    (println "dev mode")))

(defn ^:dev/after-load mount-root []
  (rf/clear-subscription-cache!)
  (let [root-el (.getElementById js/document "app")]
    (rdom/unmount-component-at-node root-el)
    (rdom/render [main/main-panel] root-el)))

(defn init []
  (dev-setup)
  (mount-root)
  (rf/dispatch-sync [::events/initialize-db])
  ;; Global listeners live alone up here, pretty much unaffected by the rest of
  ;; the app
  (.addEventListener js/window
                     "message"
                     (fn [message]
                       (cond
                         (= "yscript.graph.codeUpdated" (-> message .-data .-type))
                         (rf/dispatch [::ys-events/code-updated (-> message .-data .-text)])

                         :else
                         (println "Received unrecognized message:" (str message)))))
  (.addEventListener js/document
                     "keydown"
                     (fn [event]
                       (cond
                         (= "Escape" (.-key event))
                         (rf/dispatch [::events/escape])

                         (and (.-ctrlKey event)
                              (= "z" (.-key event)))
                         (rf/dispatch [:undo])))))
