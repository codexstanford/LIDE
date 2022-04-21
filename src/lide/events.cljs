(ns lide.events
  (:require
   [re-frame.core :as re-frame]
   [lide.db :as db]
   [day8.re-frame.tracing :refer-macros [fn-traced]]
   ))

(defn first-indexed [pred coll]
  (->> coll
       (keep-indexed (fn [idx elem]
                       (when (pred elem)
                         [idx elem])))
       first))

(re-frame/reg-event-db
 ::initialize-db
 (fn-traced [_ _]
   db/default-db))

(re-frame/reg-event-db
 ::disconnect
 (fn-traced [db [_ {[src-pred src-arg]   :src
                    [dest-pred dest-arg] :dest}]]
   (let [[dest-rule-idx dest-rule] (first-indexed #(= dest-pred (-> % :head :predicate))
                                                  (:program db))
         [src-pred-idx _] (first-indexed #(= src-pred (:predicate %))
                                         (:body dest-rule))]
     (update-in db
                [:program dest-rule-idx :body src-pred-idx :args]
                (partial map (fn [arg]
                               (if (= arg dest-arg)
                                 :unspecified
                                 arg)))))))
