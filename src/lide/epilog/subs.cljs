(ns lide.epilog.subs
  (:require
   [re-frame.core :as rf]))

(rf/reg-sub
 ::rule
 (fn [db [_ [head body-idx]]]
   (get-in db [:program :rules head body-idx])))

(rf/reg-sub
 ::query-result
 (fn [db [_ query]]
   (if (= query (:query db))
     (:query-result db)
     [])))

(rf/reg-sub
 ::matches
 (fn [db _]
   (get-in db [:program :matches])))
