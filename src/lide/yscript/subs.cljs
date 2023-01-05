(ns lide.yscript.subs
  (:require
   [re-frame.core :as rf]
   [lide.subs :as subs]
   [lide.yscript.core :as ys]
   [lide.yscript.db :as ys-db]))

(rf/reg-sub
 ::program
 (fn [db _]
   (:program db)))

(rf/reg-sub
 ::fact-values
 (fn [db _]
   (:fact-values db)))

(rf/reg-sub
 ::rule-order
 (fn [db _]
   (:rule-source-order db)))

(rf/reg-sub
 ::fact
 (fn [db [_ id]]
   (get-in db [:program :facts id])))

(rf/reg-sub
 ::statement
 (fn [db [_ [rule-name statement-idx]]]
   (get-in db [:program :rules rule-name :statements statement-idx])))

(rf/reg-sub
 ::rule
 (fn [db [_ id]]
   (get-in db [:program :rules id])))

(rf/reg-sub
 ::goal-rule
 (fn [db]
   (:goal-rule db)))

(rf/reg-sub
 ::next-for-goal
 (fn [_ _]
   [(rf/subscribe [::program])
    (rf/subscribe [::fact-values])
    (rf/subscribe [::goal-rule])])
 (fn [[program fact-values goal-rule]]
   (ys/next-for-goal-fact program
                          fact-values
                          (-> program :rules (get goal-rule) :statements (nth 0) :dest_fact :descriptor))))
