(ns lide.yscript.events
  (:require
   [instaparse.core :as insta]
   [re-frame.core :as rf]
   [day8.re-frame.undo :as undo]
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
           (println "publishing to VS Code")
           (. vs-code
              postMessage
              (clj->js {:type "programUpdated"
                        :text (ys/codify-program
                               (ys-db/populate-program
                                (get-in context [:effects :db :program])))})))
         context)))))

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

(rf/reg-event-db
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
 [updates-code
  (undo/undoable "set fact value")]
 (fn [db [_ fact-id value]]
   (-> db
       (assoc-in [:program :facts fact-id :value] value)
       (update :program
               #(ys/forward-chain %
                                  {:statements-by-required-fact (ys/statements-by-required-fact %)}
                                  fact-id)))))

(rf/reg-event-db
 ::code-updated
 (undo/undoable "update code")
 (fn [db [_ new-code]]
   (let [parse (ys/parse new-code)]
     (if (insta/failure? parse)
       ;; If the new code wasn't parsable, ignore the change
       db
       ;; Otherwise, switch to the new program
       (let [db' (->> (ys-db/ingest parse)
                      first
                      (merge db))]
         (ys-db/reconcile-ids db' db))))))
