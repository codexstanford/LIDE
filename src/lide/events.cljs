(ns lide.events
  (:require
   [clojure.edn :as edn]
   [re-frame.core :as re-frame]
   [day8.re-frame.undo :as undo]
   [day8.re-frame.tracing :refer-macros [fn-traced]]
   [lide.db :as db]
   [lide.util :as util]
   ))

(defn find-head [db predicate]
  (->> db
       :program
       :rules
       (filter #(= predicate (-> % :head :predicate)))
       first
       :head))

;; Undo/redo config

(undo/undo-config!
 {:harvest-fn
  (fn [!db]
    (:program @!db))

  :reinstate-fn
  (fn [!db val]
    (swap! !db assoc :program val))})

;; LocalStorage

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

(re-frame/reg-event-db
 ::highlight-connection
 (fn [db [_ connection]]
   (assoc db :highlighted-connection connection)))

(re-frame/reg-event-db
 ::stop-connection-highlight
 (fn [db _]
   (dissoc db :highlighted-connection)))

(re-frame/reg-event-db
 ::disconnect
 (fn [db [_ {[src-pred src-arg]   :src
             [dest-pred dest-arg] :dest}]]
   (let [[dest-rule-idx dest-rule] (util/first-indexed #(= dest-pred (-> % :head :predicate))
                                                  (-> db :program :rules))
         [src-pred-idx _] (util/first-indexed #(= src-pred (:predicate %))
                                         (:body dest-rule))
         args (get-in db [:program :rules dest-rule-idx :body src-pred-idx :args])
         updated-args (mapv (fn [arg]
                              (if (= arg dest-arg)
                                :unspecified
                                arg))
                            args)]
     (if (some #(not= % :unspecified) updated-args)
       ;; Still at least one binding for src, so keep it
       (assoc-in db [:program :rules dest-rule-idx :body src-pred-idx :args] updated-args)
       ;; No bindings remaining for src: remove it from dest's body
       (update-in db [:program :rules dest-rule-idx :body]
                  (fn [body]
                    (vec (remove #(= src-pred (:predicate %)) body))))))))

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
 ::start-connect-dest
 (fn [db [_ dest-pred]]
   (assoc db :connecting-dest dest-pred)))

;; TODO Handle name collisions
(re-frame/reg-event-db
 ::connect-src
 (fn [db [_ src-pred [src-arg-idx src-arg]]]
   (let [connecting-dest (:connecting-dest db)]
     (if-not connecting-dest
       db
       (let [updated-rules
             (mapv
              (fn [dest-rule]
                (if (not= connecting-dest (-> dest-rule :head :predicate))
                  dest-rule
                  (let [bound-src-literal
                        (first (filter #(= src-pred (:predicate %))
                                       (:body dest-rule)))

                        body-with-src-literal
                        (if bound-src-literal
                          ;; Already had source literal in body, so no need to create
                          (:body dest-rule)
                          ;; Binding source literal for the first time, need to create
                          (conj (:body dest-rule)
                                (-> (find-head db src-pred)
                                    (update :args
                                            (partial mapv (fn [_] :unspecified))))))

                        updated-body
                        (mapv
                         (fn [body-literal]
                           (if (= src-pred (:predicate body-literal))
                             ;; Add binding for matching arg
                             (update body-literal
                                     :args
                                     (fn [args]
                                       (assoc args src-arg-idx src-arg)))
                             body-literal))
                         body-with-src-literal)]
                    (assoc dest-rule :body updated-body))))
              (-> db :program :rules))]
         (-> db
             (assoc-in [:program :rules] updated-rules)
             (dissoc :connecting-dest)
             (dissoc :mouse-position)))))))

(re-frame/reg-event-fx
 ::mouse-up
 (fn [cofx [_ position]]
   ;; Mouse up could be the end of a drag, in which case we do nothing. If there
   ;; was no drag between mouse down and mouse up, that's a click, and we create
   ;; a rule.
   (let [fx (if (:dragged (:db cofx))
              []
              [[:dispatch [::create-rule position]]])]
     {:db (-> (:db cofx)
              (dissoc :dragged)
              (dissoc :dragging-rule)
              (dissoc :dragging-literal)
              (dissoc :node-drag-origin)
              (dissoc :graph-drag-origin))
      :fx fx})))

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
 ::select-rule
 (fn [db [_ rule-idx]]
   (assoc db :selected-rule-index rule-idx)))

(re-frame/reg-event-db
 ::select-literal
 (fn [db [_ literal-id]]
   (assoc db :selected-literal literal-id)))

(re-frame/reg-event-db
 ::edit-predicate
 (undo/undoable "edit predicate")
 (fn [db [_ rule-idx new-predicate]]
   (let [rule (get (-> db :program :rules) rule-idx)]
     (update-in db
                [:program :literals (:head rule)]
                (fn [literal]
                  (assoc literal :predicate new-predicate))))))

(re-frame/reg-event-db
 ::mouse-move
 (fn [db [_ event-position]]
   (cond
     (contains? db :connecting-dest)
     (assoc db :mouse-position event-position)

     (contains? db :graph-drag-origin)
     (let [dx (- (-> event-position :x)
                 (-> db :graph-drag-origin :x))
           dy (- (-> event-position :y)
                 (-> db :graph-drag-origin :y))
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
   (assoc db :graph-drag-origin position)))

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
