(ns lide.subs
  (:require
   [re-frame.core :as re-frame]))

(re-frame/reg-sub
 ::program
 (fn [db]
   (:program db)))

(re-frame/reg-sub
 ::rule-positions
 (fn [db]
   (:rule-positions db)))

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
