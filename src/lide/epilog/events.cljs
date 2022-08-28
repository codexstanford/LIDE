(ns lide.epilog.events
  (:require
   [re-frame.core :as rf]
   [lide.events :as events]
   [lide.util :as util]))

(rf/reg-event-db
 ::code-updated
 (fn [db [_ new-program-json]]
   (let [new-program
         (js->clj new-program-json :keywordize-keys true)

         ;; un-keywordize rule names
         renamed-program
         (update new-program :rules #(util/map-keys name %))]
     (update db :program #(merge % renamed-program)))))

(rf/reg-event-fx
 ::query-rule
 (fn [cofx [_ [start-position end-position]]]
   {:fx [[::events/tell-vs-code [(-> cofx :db :vs-code)
                                 {:type "queryRule"
                                  :startPosition start-position
                                  :endPosition   end-position}]]]}))

(rf/reg-event-fx
 ::negate-literal
 (fn [cofx [_ start-position]]
   {:fx [[::events/tell-vs-code [(-> cofx :db :vs-code)
                                 {:type "negateLiteral"
                                  :startPosition start-position}]]]}))
