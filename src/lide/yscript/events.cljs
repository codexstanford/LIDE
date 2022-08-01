(ns lide.yscript.events
  (:require
   [re-frame.core :as rf]
   [lide.util :as util]
   [lide.yscript.core :as ys]
   [lide.yscript.db :as ys-db]))

(rf/reg-event-db
 ::code-updated
 (fn [db [_ new-program-json]]
   (let [new-program
         (js->clj new-program-json :keywordize-keys true)

         ;; Un-keywordize rule and fact names. Note that we do *not* use
         ;; `(name %)`, as we need to tolerate slashes in the input. This is
         ;; kind of gross; I would like at some point to improve a lot of
         ;; interchange format stuff
         renamed-program
         (-> new-program
             (update :rules #(util/map-keys (fn [k] (subs (str k) 1)) %))
             (update :facts #(util/map-keys (fn [k] (subs (str k) 1)) %)))]
     (update db :program #(merge % renamed-program)))))

(rf/reg-fx
 ::tell-vs-code
 (fn [[vs-code message]]
   (. vs-code
      postMessage
      (clj->js message))))

(rf/reg-event-fx
 ::show-range
 (fn [cofx [_ range]]
   {:fx [[::tell-vs-code [(-> cofx :db :vs-code)
                          {:type "showRange"
                           :range range}]]]}))

(rf/reg-event-fx
 ::select-range
 (fn [cofx [_ range]]
   {:fx [[::tell-vs-code [(-> cofx :db :vs-code)
                          {:type "selectRange"
                           :range range}]]]}))

(rf/reg-event-db
 ::create-rule
 (fn [db [_ position]]
   (let [id (random-uuid)]
     (-> db
         (assoc-in [:program :rules id]
                   {:name (ys/name-rule (ys/rule-names (:program db)))
                    :goal false
                    :statements []})
         (assoc-in [:positions id] position)))))

(rf/reg-event-db
 ::set-fact-value
 (fn [db [_ descriptor value]]
   (let [edited-values (assoc-in (:fact-values db)
                                 [descriptor :value]
                                 value)]
     (assoc db
            :fact-values
            (ys/forward-chain (:program db)
                              edited-values
                              descriptor)))))
