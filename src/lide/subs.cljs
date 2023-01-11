(ns lide.subs
  (:require
   [lide.graph :as graph]
   [lide.util :as util]
   [re-frame.core :as rf]))

(rf/reg-sub
 ::show-saved-popup?
 (fn [db]
   (:show-saved-popup? db)))

(rf/reg-sub
 ::show-toolbar?
 (fn [db]
   (:show-toolbar? db)))

(rf/reg-sub
 ::program
 (fn [db]
   (:program db)))

(rf/reg-sub
 ::program-target
 (fn [db]
   (get-in db [:program :target])))

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
 ::defeatings
 (fn [db]
   (-> db :program :defeatings)))

(rf/reg-sub
 ::defeated-selecting-defeater
 (fn [db]
   (-> db :defeated-selecting-defeater)))

(rf/reg-sub
 ::facts
 (fn [db]
   (-> db :program :facts)))

(rf/reg-sub
 ::fact
 (fn [db [_ id]]
   (-> db
       (get-in [:program :facts id])
       (assoc :id id))))

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

;; Position/layout

;; Rendering gets a bit complex here. Some graph nodes are rendered into
;; foreignObjects, which is a) nice because we get to use HTML instead of SVG,
;; but b) not nice because we don't know ahead of time where everything is going
;; to end up, and we need to know precise positions of things to draw
;; connectors.
;;
;; So, we do a two-stage rendering process as follows:
;;  - Render into a foreignObject
;;  - Using :ref, keep track of the actual rendered element we're interested in
;;  - Using component-did-{mount,update}, dispatch an event every time the
;;    component rerenders and store that element (plus a generation number, to
;;    make re-frame detect inequality) in app-db
;;  - Subscribe to the value of this rendered element/generation pair and read
;;    precise layout information from the element to compute ::layout.
;;
;; ::layout also depends on the node's position, which can be changed by the
;; user, and is handled separately. It's important to keep reads and writes
;; strictly partitioned to avoid layout thrashing.

(rf/reg-sub
 ::position
 (fn [db [_ type id]]
   (get-in (:positions db) [type id] {:x 0 :y 0})))

(rf/reg-sub
  ::rendered
  (fn [db [_ entity-type entity-id]]
    (get-in db (conj [:rendered] entity-type entity-id))))

(rf/reg-sub
 ::layout
 (fn [[_ type id]]
   [(rf/subscribe [::program-target])
    (atom type)
    (rf/subscribe [::position type id])
    (rf/subscribe [::rendered type id])])
 (fn [[target type position {:keys [element _]}]]
   (when element
     (case [target type]
       [:yscript :rule] (graph/ys-rule-layout position element)
       [:epilog :rule] (graph/rule-layout position element)
       [:epilog :fact] (graph/fact-layout position element)))))
