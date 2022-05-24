(ns lide.core
  (:require
   [reagent.dom :as rdom]
   [re-frame.core :as re-frame]
   [lide.events :as events]
   [lide.views :as views]
   [lide.config :as config]
   ))

(defn dev-setup []
  (when config/debug?
    (println "dev mode")))

(defn ^:dev/after-load mount-root []
  (re-frame/clear-subscription-cache!)
  (let [root-el (.getElementById js/document "app")]
    (rdom/unmount-component-at-node root-el)
    (rdom/render [views/main-panel] root-el)))

(defn init []
  (dev-setup)
  (mount-root)
  (re-frame/dispatch-sync [::events/initialize-db])
  ;; Global undo key listener lives alone up here, pretty much unaffected by the
  ;; rest of the app
  (.addEventListener js/document
                     "keydown"
                     (fn [event]
                       (when (and (.-ctrlKey event)
                                  (= "z" (.-key event)))
                         (re-frame/dispatch [:undo])))))
