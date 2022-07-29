(ns lide.yscript.events
  (:require
   [instaparse.core :as insta]
   [re-frame.core :as rf]
   [day8.re-frame.undo :as undo]
   [lide.util :as util]
   [lide.yscript.core :as ys]
   [lide.yscript.db :as ys-db]))

(def updates-code
  (rf/->interceptor
   :id :updates-code

   :after
   (fn [context]
     (let [vs-code (get-in context [:effects :db :vs-code])]
       (do
         (when vs-code
           (. vs-code
              postMessage
              (clj->js {:type "programEdited"
                        :text (ys/codify-program
                               (ys-db/populate-program
                                (get-in context [:effects :db :program]))
                               (get-in context [:effects :db :rule-source-order]))})))
         context)))))

(rf/reg-fx
 ::tell-vs-code
 (fn [[vs-code message]]
   (. vs-code
      postMessage
      (clj->js message))))

(rf/reg-fx
 ::edit-source
 (fn [[vs-code range text]]
   (when vs-code
     (. vs-code
        postMessage
        (clj->js {:type "editSource"
                  :range range
                  :text text})))))

(rf/reg-fx
 ::publish-rule-positions
 (goog.functions.debounce
  (fn [[vs-code positions]]
    (when vs-code
      (. vs-code
         postMessage
         (clj->js {:type "positionsEdited"
                   :positions (or positions {})}))))
  2000))

(rf/reg-event-db
 ::positions-read
 (fn [db [_ positions]]
   (assoc db :positions positions)))

(rf/reg-event-db
 ::create-rule
 [updates-code
  (undo/undoable "create rule")]
 (fn [db [_ position]]
   (let [id (random-uuid)]
     (-> db
         (assoc-in [:program :rules id]
                   {:name (ys/name-rule (ys-db/rule-names db))
                    :goal false
                    :statements []})
         (assoc-in [:positions id] position)))))

(rf/reg-event-db
 ::set-rule-name
 [updates-code
  (undo/undoable "set rule name")]
 (fn [db [_ id name]]
   (assoc-in db [:program :rules id :name] name)))

(rf/reg-event-db
 ::add-statement
 [updates-code
  (undo/undoable "add statement")]
 (fn [db [_ rule-id type]]
   (let [id (random-uuid)]
     (-> db
         ;; TODO support if-then
         (assoc-in [:program :statements id] {:type type
                                              :dest-fact :unspecified
                                              :src-expr :unspecified})
         (update-in [:program :rules rule-id :statements] #(conj % id))))))

(rf/reg-event-db
 ::add-source-expr
 [updates-code
  (undo/undoable "add source expression")]
 (fn [db [_ statement-id type]]
   (let [fact-id (random-uuid)]
     (-> db
         (assoc-in [:program :facts fact-id] (ys/default-fact))
         (assoc-in [:program :statements statement-id :src-expr] {:type type
                                                                  :exprs [fact-id]})))))

(rf/reg-event-fx
 ::set-dest-fact
 (undo/undoable "set statement dest_fact descriptor")
 (fn [cofx [_ [rule-name statement-idx] new-descriptor]]
   (let [dest-fact-path [:program :rules rule-name :statements statement-idx :dest_fact]
         dest-fact (get-in (:db cofx) (conj dest-fact-path))]
     {:db (assoc-in (:db cofx) dest-fact-path new-descriptor)
      :fx [[::edit-source [(-> cofx :db :vs-code)
                           [(:startPosition dest-fact)
                            (:endPosition dest-fact)]
                           new-descriptor]]]})))

#_(rf/reg-event-db
 ::set-determinee-descriptor
 [updates-code
  (undo/undoable "set determinee descriptor")]
 (fn [db [_ statement-id descriptor]]
   (let [statement (get-in db [:program :statements statement-id])
         old-determinee-id (:dest-fact statement)
         old-determinee (get-in db [:program :facts old-determinee-id])
         new-determinee-id (get (ys/facts-by-descriptor (:program db)) descriptor)
         new-determinee (get-in db [:program :facts new-determinee-id])
         db' (if new-determinee
               ;; Switch determinee to fact matching `descriptor`
               (assoc-in db [:program :statements statement-id :dest-fact] new-determinee-id)
               ;; If `descriptor` doesn't match any existing fact, make a new one
               (let [fact (assoc (or old-determinee (ys/default-fact)) :descriptor descriptor)
                     fact-id (random-uuid)]
                 (-> db
                     (assoc-in [:program :facts fact-id] fact)
                     (assoc-in [:program :statements statement-id :dest-fact] fact-id))))
         orphans (ys/orphan-facts (:program db'))]
     (update-in db' [:program :facts] #(into {}
                                             (remove (fn [[fact-id _]]
                                                       (contains? orphans fact-id))
                                                     %))))))

(rf/reg-event-db
 ::set-requiree-descriptor
 [updates-code
  (undo/undoable "set requiree descriptor")]
 (fn [db [_ [statement-id & sub-st-path :as path] descriptor]]
   (let [statement (get-in db [:program :statements statement-id])
         old-requiree-id (get-in statement sub-st-path)
         old-requiree (get-in db [:program :facts old-requiree-id])
         new-requiree-id (get (ys/facts-by-descriptor (:program db)) descriptor)
         new-requiree (get-in db [:program :facts new-requiree-id])
         db' (if new-requiree
               ;; Switch requiree to fact matching `descriptor`
               (assoc-in db (concat [:program :statements] path) new-requiree-id)
               ;; If `descriptor` doesn't match any existing fact, make a new one
               (let [fact (assoc (or old-requiree (ys/default-fact)) :descriptor descriptor)
                     fact-id (random-uuid)]
                 (-> db
                     (assoc-in [:program :facts fact-id] fact)
                     (assoc-in (concat [:program :statements] path) fact-id))))
         orphans (ys/orphan-facts (:program db'))]
     (update-in db' [:program :facts] #(into {}
                                             (remove (fn [[fact-id _]]
                                                       (contains? orphans fact-id))
                                                     %))))))

(rf/reg-event-db
 ::set-fact-value
 (undo/undoable "set fact value")
 (fn [db [_ descriptor value]]
   (let [edited-values (assoc-in (:fact-values db)
                                 [descriptor :value]
                                 value)]
     (assoc db
            :fact-values
            (ys/forward-chain (:program db)
                              edited-values
                              descriptor)))))

(rf/reg-event-db
 ::code-updated
 (undo/undoable "update code")
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
