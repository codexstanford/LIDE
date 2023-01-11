(ns lide.yscript.events
  (:require
   [lide.editor :as editor]
   [lide.util :as util]
   [lide.yscript.core :as ys]
   [re-frame.core :as rf]))

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

(rf/reg-event-db
 ::facts-updated
 (fn [db [_ new-facts-json]]
   (let [facts (js->clj new-facts-json :keywordize-keys true)
         renamed-facts (util/map-keys  (fn [k] (subs (str k) 1)) facts)]
     (assoc db :fact-values renamed-facts))))

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

(rf/reg-event-fx
 ::set-fact-value
 (fn [cofx [_ descriptor value]]
   (let [vs-code (-> cofx :db :vs-code)]
     {:fx (if vs-code
            [[::editor/tell-vs-code
              [vs-code
               {:type "incorporateFact"
                :descriptor descriptor
                :value value}]]])})))

(rf/reg-event-db
 ::set-goal
 (fn [db [_ name]]
   (assoc db :goal-rule name)))
