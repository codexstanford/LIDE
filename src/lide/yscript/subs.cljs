(ns lide.yscript.subs
  (:require
   [re-frame.core :as rf]
   [lide.subs :as subs]
   [lide.yscript.core :as ys]))

(rf/reg-sub
 ::fact
 (fn [db [_ id]]
   (get-in db [:program :facts id])))

(rf/reg-sub
 ::rule
 (fn [db [_ id]]
   (get-in db [:program :rules id])))

(rf/reg-sub
 ::orphan-facts
 (fn [_ _]
   (rf/subscribe [::subs/program]))
 (fn [program]
   (ys/orphan-facts program)))

(rf/reg-sub
 ::fact-matches
 (fn [_ _]
   (rf/subscribe [::subs/program]))
 (fn [program]
   (ys/find-fact-matches program)))
