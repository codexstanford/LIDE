(ns lide.epilog.events
  (:require
   [re-frame.core :as rf]
   [lide.editor :as editor]
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
 ::create-rule
 (fn [cofx [_ position]]
   {:fx [[::editor/tell-vs-code [(-> cofx :db :vs-code)
                                 {:type "createRule"
                                  :position position}]]]}))

(rf/reg-event-fx
 ::negate-literal
 (fn [cofx [_ start-position]]
   {:fx [[::editor/tell-vs-code [(-> cofx :db :vs-code)
                                 {:type "negateLiteral"
                                  :startPosition start-position}]]]}))

(rf/reg-event-fx
 ::query
 (fn [cofx [_ query]]
   {:fx [[::editor/tell-vs-code [(-> cofx :db :vs-code)
                                 {:type "query"
                                  :query query}]]]}))

(rf/reg-event-db
 ::query-result
 (fn [db [_ query result]]
   (assoc db
          :query query
          :query-result result)))
