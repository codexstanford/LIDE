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
   (->> (get-in db [:program :rules])
        (map (fn [[id rule]]
               (assoc rule :id id)))
        (into {}))))

(rf/reg-sub
 ::rule
 (fn [db [_ id]]
   (-> db
       (get-in [:program :rules id])
       (assoc :id id))))

(rf/reg-sub
 ::populated-rules
 (fn [_ _]
   (rf/subscribe [::program]))
 (fn [program _]
   (->> (:rules program)
        (map (fn [[id rule]]
               [id (util/populate-rule program (assoc rule :id id))]))
        (into {}))))

(rf/reg-sub
 ::populated-rule
 (fn [[_ rule-id]]
   [(rf/subscribe [::program])
    (rf/subscribe [::rule rule-id])])
 (fn [[program rule]]
   (util/populate-rule program rule)))

(rf/reg-sub
 ::matches
 (fn [_ _]
   (rf/subscribe [::program]))
 (fn [program _]
   (util/all-matches program)))

(rf/reg-sub
 ::defeatings
 (fn [db]
   (-> db :program :defeatings)))

(rf/reg-sub
 ::defeating-rule-pending
 (fn [db]
   (-> db :defeating-rule-pending)))

(rf/reg-sub
 ::facts
 (fn [db]
   (-> db :program :facts)))

(rf/reg-sub
 ::rule-layout
 (fn [[_ rule-id]]
   [(rf/subscribe [::populated-rule rule-id])
    (rf/subscribe [::rule-position rule-id])])
 (fn [[rule position]]
   (assoc-in (graph/rule-layout rule) [:container :position] position)))

(rf/reg-sub
 ::rule-positions
 (fn [db]
   (:rule-positions db)))

(rf/reg-sub
 ::rule-position
 (fn [db [_ rule-id]]
   (get-in db [:rule-positions rule-id] {:x 0 :y 0})))

(rf/reg-sub
 ::literal-positions
 (fn [db]
   (:literal-positions db)))

(rf/reg-sub
 ::literal-position
 (fn [db [_ literal-id]]
   (get-in db [:literal-positions literal-id])))

(rf/reg-sub
 ::selected-rule-id
 (fn [db]
   (:selected-rule-id db)))

(rf/reg-sub
 ::selected-rule
 (fn [_ _]
   [(rf/subscribe [::rules])
    (rf/subscribe [::selected-rule-id])])
 (fn [[rules rule-id]]
   (get rules rule-id)))

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
