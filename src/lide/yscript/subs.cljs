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
 ::statement
 (fn [db [_ id]]
   (get-in db [:program :statements id])))

(rf/reg-sub
 ::rule
 (fn [db [_ id]]
   (get-in db [:program :rules id])))

(rf/reg-sub
 ::rules-by-statement
 (fn [_ _]
   (rf/subscribe [::subs/program]))
 (fn [program]
   (ys/rules-by-statement program)))

(rf/reg-sub
 ::rule-containing-statement
 (fn [[_ statement-id]]
   [(atom statement-id)
    (rf/subscribe [::rules-by-statement])])
 (fn [[statement-id rules-by-statement]]
   (get rules-by-statement statement-id)))

(rf/reg-sub
 ::statements-by-determined-fact
 (fn [_ _]
   (rf/subscribe [::subs/program]))
 (fn [program]
   (ys/statements-by-determined-fact program)))

(rf/reg-sub
 ::statements-determining-fact
 (fn [[_ fact-id]]
   [(atom fact-id)
    (rf/subscribe [::statements-by-determined-fact])])
 (fn [[fact-id statements-by-fact]]
   (get statements-by-fact fact-id)))

(rf/reg-sub
 ::statements-by-required-fact
 (fn [_ _]
   (rf/subscribe [::subs/program]))
 (fn [program]
   (ys/statements-by-required-fact program)))

(rf/reg-sub
 ::statements-requiring-fact
 (fn [[_ fact-id]]
   [(atom fact-id)
    (rf/subscribe [::statements-by-required-fact])])
 (fn [[fact-id st-by-fact]]
   (get st-by-fact fact-id)))

#_(rf/reg-sub
 ::orphan-facts
 (fn [_ _]
   (rf/subscribe [::subs/program]))
 (fn [program]
   (ys/orphan-facts program)))
