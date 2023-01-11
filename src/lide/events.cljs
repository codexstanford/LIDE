(ns lide.events
  (:require
   [clojure.edn :as edn]
   [re-frame.core :as rf]
   [day8.re-frame.undo :as undo]
   [lide.editor :as editor]
   [lide.util :as util]
   [lide.epilog.core :as epilog]
   [lide.epilog.db :as epilog-db]
   [lide.epilog.events :as el-events]
   [lide.yscript.core :as ys]
   [lide.yscript.db :as ys-db]
   [lide.yscript.events :as ys-events]))

;; General purpose escape/cancel handler

(rf/reg-event-db
 ::escape
 (fn [db _]
   (dissoc db :defeated-selecting-defeater)))

;; Undo/redo config

(undo/undo-config!
 {:harvest-fn
  (fn [!db]
    (:program @!db))

  :reinstate-fn
  (fn [!db val]
    (swap! !db assoc :program val))})

;; Delay effects

(rf/reg-fx
 :timeout
 (fn [{:keys [event delay-ms]}]
   (js/setTimeout (fn []
                    (rf/dispatch event))
                  delay-ms)))

;; Persistent state

(rf/reg-fx
 ::set-local-storage
 (fn [[key val]]
   (-> js/window
       (.-localStorage)
       (.setItem key val))))

(rf/reg-event-fx
 ::save
 (fn [cofx _]
   {:db (assoc (:db cofx) :show-saved-popup? true)
    :fx [[::set-local-storage ["lide.state" (-> (:db cofx)
                                                (select-keys [:program
                                                              :positions
                                                              :collapsed-literals
                                                              :graph-transform])
                                                pr-str)]]]
    :timeout {:event [::hide-saved-popup]
              :delay-ms 2000}}))

(rf/reg-event-db
 ::hide-saved-popup
 (fn [db _]
   (dissoc db :show-saved-popup?)))

(rf/reg-cofx
 ::saved-state
 (fn [cofx]
   (assoc cofx
          ::saved-state
          (-> js/localStorage
              (.getItem "lide.state")
              edn/read-string))))

;; Persistent positions

;; TODO debounce this
(rf/reg-event-fx
 ::publish-rule-positions
 (fn [cofx]
   (let [vs-code (-> cofx :db :vs-code)
         target (-> cofx :db :program :target)
         positions (or (-> cofx :db :positions) {})]
     {:fx (if vs-code
            [[::editor/tell-vs-code
              [vs-code
               {:type "positionsEdited"
                :positions (case target
                             :epilog (epilog/sanitize-positions positions)
                             :yscript positions)}]]]
            [])})))

(rf/reg-event-db
 ::positions-read
 (fn [db [_ positions]]
   (case (get-in db [:program :target])
     :epilog (assoc db :positions (epilog/parse-positions positions))
     :yscript (assoc db :positions (ys/parse-positions positions)))))

(rf/reg-event-fx
 ::initialize-db
 (fn [cofx [_ target-language]]
   {:db (merge (:db cofx)
               {:graph-transform (util/dom-matrix-to-vals (js/DOMMatrixReadOnly.))}
               (case target-language
                 :epilog epilog-db/initial-db
                 :yscript ys-db/initial-db))}))

(rf/reg-event-db
 ::vs-code-api
 (fn [db [_ vs-code]]
   (assoc db :vs-code vs-code)))

(rf/reg-event-fx
 ::app-ready
 (fn [cofx _]
   {:fx [[::editor/tell-vs-code [(-> cofx :db :vs-code)
                                 {:type "appReady"}]]]}))

;; Connections

(rf/reg-event-db
 ::highlight-connection
 (fn [db [_ connection]]
   (assoc db :highlighted-connection connection)))

(rf/reg-event-db
 ::stop-connection-highlight
 (fn [db _]
   (dissoc db :highlighted-connection)))

(rf/reg-event-db
 ::add-argument
 (undo/undoable "add argument")
 (fn [db [_ rule-id]]
   (let [head-id (get-in db [:program :rules rule-id :head])]
     (update-in db
                [:program :literals head-id :args]
                (fn [args]
                  (conj args "arg"))))))

(rf/reg-event-db
 ::add-literal-argument
 (undo/undoable "add literal argument")
 (fn [db [_ literal-id]]
   (update-in db [:program :literals literal-id :args] #(conj % "new"))))

(rf/reg-event-fx
 ::mouse-up
 (fn [cofx [_ position]]
   {:db (-> (:db cofx)
            (dissoc :dragged)
            (dissoc :dragging-id)
            (dissoc :dragging-type)
            (dissoc :drag-origin)
            (dissoc :mouse-down-graph))
    :fx [(when (and (:mouse-down-graph (:db cofx))
                    (not (:dragged (:db cofx))))
           ;; If mouse down was on the graph background, and there was no drag
           ;; before mouse up, that's a click on the graph background. In
           ;; response, we create a rule.
           (case (get-in (:db cofx) [:program :target])
             :epilog [:dispatch [::el-events/create-rule position]]
             :yscript [:dispatch [::ys-events/create-rule position]]))]}))

(rf/reg-event-db
 ::toggle-collapse-literal
 (fn [db [_ literal-id]]
   (update-in db [:program :literals literal-id :collapsed] not)))


(rf/reg-event-fx
 ::mouse-move
 (fn [cofx [_ event local-position]]
   (let [db (:db cofx)]
     (cond
       (contains? db :defeated-selecting-defeater)
       {:db (assoc db :mouse-position local-position)}

       (contains? db :mouse-down-graph)
       (let [dx (- (-> event .-clientX)
                   (-> db :mouse-down-graph :x))
             dy (- (-> event .-clientY)
                   (-> db :mouse-down-graph :y))
             translation-screen (.translate (js/DOMMatrixReadOnly.) dx dy)]
         {:db
          (-> db
              (assoc :dragged true)
              (assoc :mouse-down-graph {:x (.-clientX event)
                                        :y (.-clientY event)})
              (assoc :graph-transform
                     (util/dom-matrix-to-vals
                      (.preMultiplySelf (util/dom-matrix-from-vals (:graph-transform db))
                                        translation-screen))))})

       (contains? db :dragging-id)
       (let [dx (- (-> local-position :x)
                   (-> db :drag-origin :x))
             dy (- (-> local-position :y)
                   (-> db :drag-origin :y))]
         {:db
          (-> db
              (update-in [:positions :rule (:dragging-id db)]
                         (fn [position]
                           (merge-with +
                                       (or position {:x 0, :y 0})
                                       {:x dx, :y dy})))
              (assoc :drag-origin local-position)
              (assoc :dragged true))

          :fx [[:dispatch [::publish-rule-positions]]]})

       :else {:db db}))))

(rf/reg-event-db
 ::mouse-down-graph-bg
 (fn [db [_ event]]
   (assoc db :mouse-down-graph {:x (.-clientX event)
                                :y (.-clientY event)})))

(rf/reg-event-db
 ::scroll-graph
 (fn [db [_ scroll-event]]
   (let [event-position {:x (-> scroll-event .-nativeEvent .-offsetX)
                         :y (-> scroll-event .-nativeEvent .-offsetY)}
         scale (if (> (.-deltaY scroll-event) 0)
                 0.8
                 1.25)
         graph-transform (util/dom-matrix-from-vals (:graph-transform db))
         zoom-matrix (-> (js/DOMMatrix.)
                         (.translateSelf (:x event-position)
                                         (:y event-position))
                         (.scaleSelf scale scale)
                         (.translateSelf (- (:x event-position))
                                         (- (:y event-position))))]
     (assoc db :graph-transform (util/dom-matrix-to-vals
                                 (.preMultiplySelf graph-transform zoom-matrix))))))

(rf/reg-event-db
 ::start-drag
 (fn [db [_ position id type]]
   (-> db
       (assoc :drag-origin position)
       (assoc :dragging-id id)
       (assoc :dragging-type type))))

;; Defeasibility

;; TODO Figure out undo for the two-stage process of adding defeats

(rf/reg-event-db
 ::defeated-selecting-defeater
 (fn [db [_ defeated-id]]
   (assoc db :defeated-selecting-defeater defeated-id)))

(rf/reg-event-db
 ::select-defeater
 (fn [db [_ defeater-id]]
   (let [defeated-id (:defeated-selecting-defeater db)]
     (cond
       (nil? defeated-id)
       db

       ;; Odd thing to do, probably interpret it as a cancel
       (= defeated-id defeater-id)
       (dissoc db :defeated-selecting-defeater)

       :else
       (-> db
           (update-in [:program :defeatings] #(conj % {:defeated defeated-id
                                                       :defeater defeater-id}))
           (dissoc :defeated-selecting-defeater))))))

(rf/reg-event-db
 ::remove-defeat
 (fn [db [_ defeat]]
   (update-in db [:program :defeatings] #(disj % defeat))))

;; Rendered element feedback (see subs.cljs for details)

(rf/reg-event-db
 ::rendered
 (fn [db [_ entity-type entity-id element generation]]
   (assoc-in db (conj [:rendered] entity-type entity-id) {:element element
                                                          :generation generation})))
