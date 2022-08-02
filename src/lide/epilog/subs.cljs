(ns lide.epilog.subs
  (:require
   [re-frame.core :as rf]))

(rf/reg-sub
 ::rule
 (fn [db [_ rule-name]]
   (get-in db [:program :rules rule-name])))
