(ns lide.subs
  (:require
   [lide.graph :as graph]
   [lide.util :as util]
   [lide.epilog :as epilog]
   [re-frame.core :as rf]))

(rf/reg-sub
 ::show-saved-popup?
 (fn [db] (:show-saved-popup? db)))

(rf/reg-sub
 ::program
 (fn [db]
   (:program db)))

(rf/reg-sub
 ::literal
 (fn [db [_ id]]
   (get-in db [:program :literals id])))

(rf/reg-sub
 ::rules
 (fn [db]
   (-> db :program :rules)))

(rf/reg-sub
 ::rule
 (fn [db [_ idx]]
   (get-in db [:program :rules idx])))

(rf/reg-sub
 ::populated-rules
 (fn [_ _]
   (rf/subscribe [::program]))
 (fn [program _]
   (->> (:rules program)
        (mapv #(util/populate-rule program %)))))

(rf/reg-sub
 ::populated-rule
 (fn [[_ rule-idx]]
   [(rf/subscribe [::program])
    (rf/subscribe [::rule rule-idx])])
 (fn [[program rule]]
   (util/populate-rule program rule)))

(rf/reg-sub
 ::matches
 (fn [_ _]
   (rf/subscribe [::program]))
 (fn [program _]
   (util/all-matches program)))

(rf/reg-sub
 ::rule-layout
 (fn [[_ rule-idx]]
   [(rf/subscribe [::populated-rule rule-idx])
    (rf/subscribe [::rule-position rule-idx])])
 (fn [[rule position]]
   (assoc-in (graph/rule-layout rule) [:container :position] position)))

(rf/reg-sub
 ::rule-positions
 (fn [db]
   (:rule-positions db)))

(rf/reg-sub
 ::rule-position
 (fn [db [_ rule-idx]]
   (get-in db [:rule-positions rule-idx])))

(rf/reg-sub
 ::literal-positions
 (fn [db]
   (:literal-positions db)))

(rf/reg-sub
 ::literal-position
 (fn [db [_ literal-id]]
   (get-in db [:literal-positions literal-id])))

(rf/reg-sub
 ::selected-rule-index
 (fn [db]
   (:selected-rule-index db)))

(rf/reg-sub
 ::selected-rule
 (fn [_ _]
   [(rf/subscribe [::rules])
    (rf/subscribe [::selected-rule-index])])
 (fn [[rules rule-idx]]
   (get rules rule-idx)))

(rf/reg-sub
 ::highlighted-connection
 (fn [db]
   (:highlighted-connection db)))

(rf/reg-sub
 ::connecting-dest
 (fn [db]
   (:connecting-dest db)))

(rf/reg-sub
 ::mouse-position
 (fn [db]
   (:mouse-position db)))

(rf/reg-sub
 ::graph-transform
 (fn [db]
   (:graph-transform db)))
