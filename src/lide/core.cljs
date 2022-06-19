(ns lide.core
  (:require
   [reagent.dom :as rdom]
   [re-frame.core :as rf]
   [lide.events :as events]
   [lide.views :as views]
   [lide.config :as config]
   ))

(defn dev-setup []
  (when config/debug?
    (println "dev mode")))

(defn ^:dev/after-load mount-root []
  (rf/clear-subscription-cache!)
  (let [root-el (.getElementById js/document "app")]
    (rdom/unmount-component-at-node root-el)
    (rdom/render [views/main-panel] root-el)))

(defn init []
  (dev-setup)
  (mount-root)
  (rf/dispatch-sync [::events/initialize-db])
  ;; Global key listeners live alone up here, pretty much unaffected by the rest
  ;; of the app
  (.addEventListener js/document
                     "keydown"
                     (fn [event]
                       (cond
                         (= "Escape" (.-key event))
                         (rf/dispatch [::events/escape])

                         (and (.-ctrlKey event)
                              (= "z" (.-key event)))
                         (rf/dispatch [:undo])))))
