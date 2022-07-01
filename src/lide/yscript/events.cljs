(ns lide.yscript.events
  (:require
   [re-frame.core :as rf]
   [lide.yscript.core :as ys]))

(rf/reg-event-db
 ::set-fact-value
 (fn [db [_ fact-id value]]
   (-> db
    (assoc-in [:program :facts fact-id :value] value)
    (update :program
            #(ys/forward-chain %
                               {:statements-by-required-fact (ys/statements-by-required-fact %)}
                               fact-id)))))
