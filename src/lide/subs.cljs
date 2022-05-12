(ns lide.subs
  (:require
   [lide.util :as util]
   [lide.epilog :as epilog]
   [re-frame.core :as re-frame]))

(re-frame/reg-sub
 ::program
 (fn [db]
   (:program db)))

(re-frame/reg-sub
 ::literal
 (fn [db [_ id]]
   (get-in db [:program :literals id])))

(re-frame/reg-sub
 ::rules
 (fn [db]
   (-> db :program :rules)))

(re-frame/reg-sub
 ::rule
 (fn [db [_ idx]]
   (get-in db [:program :rules idx])))

(re-frame/reg-sub
 ::populated-rules
 (fn [_ _]
   (re-frame/subscribe [::program]))
 (fn [program _]
   (->> (:rules program)
        (mapv #(util/populate-rule program %)))))

(re-frame/reg-sub
 ::populated-rule
 (fn [_ _]
   (re-frame/subscribe [::populated-rules]))
 (fn [rules [_ rule-idx]]
   (get rules rule-idx)))

(re-frame/reg-sub
 ::rule-positions
 (fn [db]
   (:rule-positions db)))

(re-frame/reg-sub
 ::rule-position
 (fn [db [_ rule-idx]]
   (get-in db [:rule-positions rule-idx])))

(re-frame/reg-sub
 ::literal-positions
 (fn [db]
   (:literal-positions db)))

(re-frame/reg-sub
 ::literal-position
 (fn [db [_ literal-id]]
   (get-in db [:literal-positions literal-id])))

(re-frame/reg-sub
 ::selected-rule-index
 (fn [db]
   (:selected-rule-index db)))

(re-frame/reg-sub
 ::selected-rule
 (fn [_ _]
   [(re-frame/subscribe [::rules])
    (re-frame/subscribe [::selected-rule-index])])
 (fn [[rules rule-idx]]
   (get rules rule-idx)))

(re-frame/reg-sub
 ::highlighted-connection
 (fn [db]
   (:highlighted-connection db)))

(re-frame/reg-sub
 ::connecting-dest
 (fn [db]
   (:connecting-dest db)))

(re-frame/reg-sub
 ::mouse-position
 (fn [db]
   (:mouse-position db)))

(re-frame/reg-sub
 ::graph-transform
 (fn [db]
   (:graph-transform db)))
