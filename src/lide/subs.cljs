(ns lide.subs
  (:require
   [re-frame.core :as re-frame]))

(re-frame/reg-sub
 ::program
 (fn [db]
   (:program db)))

(re-frame/reg-sub
 ::connecting-dest
 (fn [db]
   (:connecting-dest db)))

(re-frame/reg-sub
 ::mouse-position
 (fn [db]
   (:mouse-position db)))
