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

(defn handle-message [^js event]
  (let [message (-> event .-data)]
    (case (.-type message)
      "lide.initForLanguage"
      (rf/dispatch [::events/initialize-db (keyword (.-language message))])

      "lide.codeUpdated.epilog"
      (rf/dispatch [::epilog-events/code-updated (.-model message)])

      "lide.queryResult"
      (let [message (js->clj message :keywordize-keys true)]
        (rf/dispatch [::epilog-events/query-result (:query message) (:results message)]))

      "lide.codeUpdated.yscript"
      (rf/dispatch [::ys-events/code-updated (.-model message)])

      "lide.positionsRead"
      (rf/dispatch [::events/positions-read (.-positions message)])

      "lide.factsUpdated"
      (rf/dispatch [::ys-events/facts-updated (.-facts message)])

      ;; Sometimes our frame might receive other messages from who knows where.
      ;; Just ignore them
      nil)))

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
  (if (.-acquireVsCodeApi js/window)
    (do
      (rf/dispatch-sync [::events/vs-code-api (. js/window acquireVsCodeApi)])
      (rf/dispatch-sync [::events/app-ready]))
    (do
      (rf/dispatch-sync [::events/initialize-db :epilog])
      (rf/dispatch-sync [::epilog-events/code-updated (js/JSON.parse "{\"rules\":{\"p(a,b)\":[{\"head\":{\"nodeId\":5268608,\"negated\":false,\"predicate\":{\"text\":\"p\",\"startPosition\":{\"row\":0,\"column\":0},\"endPosition\":{\"row\":0,\"column\":1}},\"args\":[{\"type\":\"constant\",\"text\":\"a\",\"startPosition\":{\"row\":0,\"column\":2},\"endPosition\":{\"row\":0,\"column\":3}},{\"type\":\"constant\",\"text\":\"b\",\"startPosition\":{\"row\":0,\"column\":5},\"endPosition\":{\"row\":0,\"column\":6}}]},\"body\":[{\"nodeId\":5268520,\"negated\":false,\"predicate\":{\"text\":\"q\",\"startPosition\":{\"row\":0,\"column\":11},\"endPosition\":{\"row\":0,\"column\":12}},\"args\":[{\"type\":\"constant\",\"text\":\"a\",\"startPosition\":{\"row\":0,\"column\":13},\"endPosition\":{\"row\":0,\"column\":14}}]}]}],\"p(a,b,CoolVariable)\":[{\"head\":{\"nodeId\":5270016,\"negated\":false,\"predicate\":{\"text\":\"p\",\"startPosition\":{\"row\":1,\"column\":0},\"endPosition\":{\"row\":1,\"column\":1}},\"args\":[{\"type\":\"constant\",\"text\":\"a\",\"startPosition\":{\"row\":1,\"column\":2},\"endPosition\":{\"row\":1,\"column\":3}},{\"type\":\"constant\",\"text\":\"b\",\"startPosition\":{\"row\":1,\"column\":5},\"endPosition\":{\"row\":1,\"column\":6}},{\"type\":\"variable\",\"text\":\"CoolVariable\",\"startPosition\":{\"row\":1,\"column\":8},\"endPosition\":{\"row\":1,\"column\":20}}]},\"body\":[{\"nodeId\":5269928,\"negated\":false,\"predicate\":{\"text\":\"q\",\"startPosition\":{\"row\":1,\"column\":25},\"endPosition\":{\"row\":1,\"column\":26}},\"args\":[{\"type\":\"variable\",\"text\":\"CoolVariable\",\"startPosition\":{\"row\":1,\"column\":27},\"endPosition\":{\"row\":1,\"column\":39}}]}]}],\"q(X)\":[{\"head\":{\"nodeId\":5270680,\"negated\":false,\"predicate\":{\"text\":\"q\",\"startPosition\":{\"row\":3,\"column\":0},\"endPosition\":{\"row\":3,\"column\":1}},\"args\":[{\"type\":\"variable\",\"text\":\"X\",\"startPosition\":{\"row\":3,\"column\":2},\"endPosition\":{\"row\":3,\"column\":3}}]},\"body\":[{\"nodeId\":5270592,\"negated\":false,\"predicate\":{\"text\":\"r\",\"startPosition\":{\"row\":3,\"column\":8},\"endPosition\":{\"row\":3,\"column\":9}},\"args\":[]}]}]},\"matches\":[{\"subgoal\":[\"p(a,b)\",0,0],\"rule\":[\"q(X)\",0]},{\"subgoal\":[\"p(a,b,CoolVariable)\",0,0],\"rule\":[\"q(X)\",0]}]}")]))))
