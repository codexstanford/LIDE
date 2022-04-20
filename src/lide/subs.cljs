(ns lide.subs
  (:require
   [re-frame.core :as re-frame]))

(re-frame/reg-sub
 ::program
 (fn [db]
   (:program db)))
