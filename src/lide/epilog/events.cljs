(ns lide.epilog.events
  (:require
   [re-frame.core :as rf]
   [lide.util :as util]))

(rf/reg-event-db
 ::code-updated
 (fn [db [_ new-program-json]]
   (let [new-program
         (js->clj new-program-json :keywordize-keys true)

         ;; un-keywordize rule and fact names
         renamed-program
         (-> new-program
             (update :rules #(util/map-keys name %))
             (update :facts #(util/map-keys name %)))]
     (update db :program #(merge % renamed-program)))))
