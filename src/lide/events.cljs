(ns lide.events
  (:require
   [clojure.edn :as edn]
   [clojure.string :as string]
   [re-frame.core :as re-frame]
   [day8.re-frame.undo :as undo]
   [day8.re-frame.tracing :refer-macros [fn-traced]]
   [lide.db :as db]
   [lide.util :as util]
   ))

;; Undo/redo config

(undo/undo-config!
 {:harvest-fn
  (fn [!db]
    (:program @!db))

  :reinstate-fn
  (fn [!db val]
    (swap! !db assoc :program val))})

;; Persistent state

(re-frame/reg-fx
 ::set-local-storage
 (fn [[key val]]
   (-> js/window
       (.-localStorage)
       (.setItem key val))))

(re-frame/reg-event-fx
 ::save
 (fn [cofx _]
   {:fx [[::set-local-storage ["lide.state" (pr-str (:db cofx))]]]}))

(re-frame/reg-cofx
 ::saved-state
 (fn [cofx key]
   (assoc cofx
          ::saved-state
          (-> js/localStorage
              (.getItem "lide.state")
              edn/read-string))))

(re-frame/reg-event-fx
 ::initialize-db
 [(re-frame/inject-cofx ::saved-state)]
 (fn [cofx _]
   {:db (or (::saved-state cofx)
            db/default-db)}))

;; Connections

(re-frame/reg-event-db
 ::highlight-connection
 (fn [db [_ connection]]
   (assoc db :highlighted-connection connection)))

(re-frame/reg-event-db
 ::stop-connection-highlight
 (fn [db _]
   (dissoc db :highlighted-connection)))

(re-frame/reg-event-db
 ::add-argument
 (undo/undoable "add argument")
 (fn [db [_ rule-idx]]
   (let [head-id (get-in db [:program :rules rule-idx :head])]
     (update-in db
                [:program :literals head-id :args]
                (fn [args]
                  (conj args "arg"))))))

(re-frame/reg-event-db
 ::add-literal-argument
 (undo/undoable "add literal argument")
 (fn [db [_ literal-id]]
   (update-in db [:program :literals literal-id :args] conj "new")))

(re-frame/reg-event-db
 ::start-connect-dest
 (fn [db [_ dest-pred]]
   (assoc db :connecting-dest dest-pred)))

(re-frame/reg-event-fx
 ::mouse-up
 (fn [cofx [_ position]]
   {:db (-> (:db cofx)
            (dissoc :dragged)
            (dissoc :dragging-rule)
            (dissoc :dragging-literal)
            (dissoc :node-drag-origin)
            (dissoc :mouse-down-graph))
    :fx (cond
          ;; If mouse down was on the graph background, and there was no drag
          ;; before mouse up, that's a click on the graph background. In
          ;; response, we create a rule.
          (and (:mouse-down-graph (:db cofx))
               (not (:dragged (:db cofx))))
          [[:dispatch [::create-rule position]]]

          :else
          [])}))

(re-frame/reg-event-db
 ::create-rule
 (undo/undoable "create rule")
 (fn [db [_ position]]
   (let [new-head-id (random-uuid)
         new-idx (-> db :program :rules count)]
     (-> db
         (assoc-in [:program :literals new-head-id]
                   {:predicate "new"})
         (update-in [:program :rules]
                    (fn [rules]
                      (conj rules {:head new-head-id})))
         (update :rule-positions
                 (fn [positions]
                   (conj positions [new-idx position])))))))

(re-frame/reg-event-db
 ::add-body-literal
 (undo/undoable "add body literal")
 (fn [db [_ rule-idx]]
   (let [new-literal-id (random-uuid)]
     (-> db
         (assoc-in [:program :literals new-literal-id] {:predicate "new"})
         (update-in [:program :rules rule-idx :body] conj new-literal-id)))))

(re-frame/reg-event-db
 ::select-rule
 (fn [db [_ rule-idx]]
   (assoc db :selected-rule-index rule-idx)))

(re-frame/reg-event-db
 ::select-literal
 (fn [db [_ literal-id]]
   (assoc db :selected-literal literal-id)))

(re-frame/reg-event-db
 ::edit-literal-predicate
 (undo/undoable "edit literal predicate")
 (fn [db [_ literal-id new-predicate]]
   (if (string/blank? new-predicate)
     (update db :program #(util/remove-literal % literal-id))
     (assoc-in db [:program :literals literal-id :predicate] new-predicate))))

(re-frame/reg-event-db
 ::edit-literal-arg
 (undo/undoable "edit literal arg")
 (fn [db [_ literal-id arg-idx new-arg]]
   (if (string/blank? new-arg)
     (update-in db [:program :literals literal-id :args] #(util/vector-remove % arg-idx))
     (assoc-in db [:program :literals literal-id :args arg-idx] new-arg))))

(re-frame/reg-event-db
 ::edit-head-predicate
 (undo/undoable "edit head predicate")
 (fn [db [_ rule-idx new-predicate]]
   (update-in db
              [:program :rules]
              (fn [rules]
                (if (string/blank? new-predicate)
                  ;; TODO Should also delete body literals that don't appear in any other rules
                  (util/vector-remove rules rule-idx)
                  (assoc-in rules [rule-idx :head :predicate] new-predicate))))))

(re-frame/reg-event-db
 ::edit-head-arg
 (undo/undoable "edit head arg")
 (fn [db [_ rule-idx arg-idx new-arg]]
   (let [rule (get-in db [:program :rules rule-idx])]
     (update-in db
                [:program :literals (:head rule) :args]
                (fn [args]
                  (if (string/blank? new-arg)
                    ;; XXX Seems not to work quite right, rule heads are
                    ;; rendered with extra space after losing an argument
                    (util/vector-remove args arg-idx)
                    (assoc args arg-idx new-arg)))))))

(re-frame/reg-event-db
 ::mouse-move
 (fn [db [_ event-position]]
   (cond
     (contains? db :connecting-dest)
     (assoc db :mouse-position event-position)

     ;; XXX There's a bug here that I haven't quite figured out: When zoomed way
     ;; out, dragging causes the graph to jump around wildly. I think it's
     ;; related to the graph transform changing but the drag origin staying
     ;; the same?
     (contains? db :mouse-down-graph)
     (let [dx (- (-> event-position :x)
                 (-> db :mouse-down-graph :x))
           dy (- (-> event-position :y)
                 (-> db :mouse-down-graph :y))
           graph-transform (util/dom-matrix-from-vals (:graph-transform db))
           translate-matrix (.translateSelf (js/DOMMatrix.) dx dy)]
       (-> db
           (assoc :dragged true)
           (assoc :graph-transform (util/dom-matrix-to-vals
                                    (.preMultiplySelf graph-transform translate-matrix)))))

     (contains? db :dragging-literal)
     (let [dx (- (-> event-position :x)
                 (-> db :node-drag-origin :x))
           dy (- (-> event-position :y)
                 (-> db :node-drag-origin :y))]
       (-> db
           (assoc :dragged true)
           (update-in [:literal-positions (:dragging-literal db)]
                      (fn [position]
                        (-> position
                            (update :x #(+ % dx))
                            (update :y #(+ % dy)))))
           (assoc :node-drag-origin event-position)))

     (contains? db :dragging-rule)
     (let [dx (- (-> event-position :x)
                 (-> db :node-drag-origin :x))
           dy (- (-> event-position :y)
                 (-> db :node-drag-origin :y))
           dragging-rule-pred (-> db :dragging-rule :head :predicate)]
       (-> db
           (assoc :dragged true)
           (update-in [:rule-positions (:dragging-rule db)]
                      (fn [position]
                        (-> position
                            (update :x #(+ % dx))
                            (update :y #(+ % dy)))))
           (assoc :node-drag-origin event-position)))

     :else db)))

(re-frame/reg-event-db
 ::mouse-down-graph-bg
 (fn [db [_ position]]
   (assoc db :mouse-down-graph position)))

(re-frame/reg-event-db
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

(re-frame/reg-event-db
 ::start-drag-literal
 (fn [db [_ position literal-id]]
   (-> db
       (assoc :dragging-literal literal-id)
       (assoc :node-drag-origin position))))

(re-frame/reg-event-db
 ::start-drag-rule
 (fn [db [_ position rule-id]]
   (-> db
       (assoc :dragging-rule rule-id)
       (assoc :node-drag-origin position))))
