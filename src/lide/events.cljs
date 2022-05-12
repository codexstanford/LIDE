(ns lide.events
  (:require
   [re-frame.core :as re-frame]
   [lide.db :as db]
   [lide.util :as util]
   [day8.re-frame.tracing :refer-macros [fn-traced]]
   ))

(defn offset-position [evt]
  {:x (-> evt .-nativeEvent .-offsetX)
   :y (-> evt .-nativeEvent .-offsetY)})

(defn first-indexed [pred coll]
  (->> coll
       (keep-indexed (fn [idx elem]
                       (when (pred elem)
                         [idx elem])))
       first))

(defn find-head [db predicate]
  (->> db
       :program
       :rules
       (filter #(= predicate (-> % :head :predicate)))
       first
       :head))

(re-frame/reg-event-db
 ::initialize-db
 (fn [_ _]
   db/default-db))

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
   (let [[dest-rule-idx dest-rule] (first-indexed #(= dest-pred (-> % :head :predicate))
                                                  (-> db :program :rules))
         [src-pred-idx _] (first-indexed #(= src-pred (:predicate %))
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

(re-frame/reg-event-db
 ::create-node
 (fn [db [_ mouse-event]]
   (let [new-idx (-> db :program :rules count)
         position (offset-position mouse-event)]
     (-> db
         (update-in [:program :rules]
                    (fn [rules]
                      (conj rules {:head {:predicate "new"}})))
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
 (fn [db [_ rule-idx new-predicate]]
   (update-in db
              [:program :rules rule-idx]
              #(assoc-in % [:head :predicate] new-predicate))))

(re-frame/reg-event-db
 ::mouse-up
 (fn [db _]
   (-> db
       (dissoc :dragging-rule)
       (dissoc :dragging-literal)
       (dissoc :node-drag-origin)
       (dissoc :graph-drag-origin))))

(re-frame/reg-event-db
 ::mouse-move
 (fn [db [_ mouse-event]]
   (let [event-position (offset-position mouse-event)]
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
             (assoc :graph-transform (util/dom-matrix-to-vals
                                      (.preMultiplySelf graph-transform translate-matrix)))
             (assoc :graph-drag-origin event-position)))

       (contains? db :dragging-literal)
       (let [dx (- (-> event-position :x)
                   (-> db :node-drag-origin :x))
             dy (- (-> event-position :y)
                   (-> db :node-drag-origin :y))]
         (-> db
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
             (update-in [:rule-positions (:dragging-rule db)]
                        (fn [position]
                          (-> position
                              (update :x #(+ % dx))
                              (update :y #(+ % dy)))))
             (assoc :node-drag-origin event-position)))

       :else db))))

(re-frame/reg-event-db
 ::start-drag-graph
 (fn [db [_ mouse-event]]
   (assoc db :graph-drag-origin {:x (-> mouse-event .-nativeEvent .-offsetX)
                                 :y (-> mouse-event .-nativeEvent .-offsetY)})))

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
 (fn [db [_ mouse-event literal-id]]
   (-> db
       (assoc :dragging-literal literal-id)
       (assoc :node-drag-origin {:x (-> mouse-event .-nativeEvent .-offsetX)
                                 :y (-> mouse-event .-nativeEvent .-offsetY)}))))

(re-frame/reg-event-db
 ::start-drag-rule
 (fn [db [_ mouse-event rule-id]]
   (-> db
       (assoc :dragging-rule rule-id)
       (assoc :node-drag-origin {:x (-> mouse-event .-nativeEvent .-offsetX)
                                 :y (-> mouse-event .-nativeEvent .-offsetY)}))))
