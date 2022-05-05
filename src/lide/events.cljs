(ns lide.events
  (:require
   [re-frame.core :as re-frame]
   [lide.db :as db]
   [day8.re-frame.tracing :refer-macros [fn-traced]]
   ))

(defn first-indexed [pred coll]
  (->> coll
       (keep-indexed (fn [idx elem]
                       (when (pred elem)
                         [idx elem])))
       first))

(defn find-head [db predicate]
  (->> db
       :program
       (filter #(= predicate (-> % :head :predicate)))
       first
       :head))

(re-frame/reg-event-db
 ::initialize-db
 (fn [_ _]
   db/default-db))

(re-frame/reg-event-db
 ::disconnect
 (fn [db [_ {[src-pred src-arg]   :src
             [dest-pred dest-arg] :dest}]]
   (let [[dest-rule-idx dest-rule] (first-indexed #(= dest-pred (-> % :head :predicate))
                                                  (:program db))
         [src-pred-idx _] (first-indexed #(= src-pred (:predicate %))
                                         (:body dest-rule))
         args (get-in db [:program dest-rule-idx :body src-pred-idx :args])
         updated-args (mapv (fn [arg]
                              (if (= arg dest-arg)
                                :unspecified
                                arg))
                            args)]
     (if (some #(not= % :unspecified) updated-args)
       ;; Still at least one binding for src, so keep it
       (assoc-in db [:program dest-rule-idx :body src-pred-idx :args] updated-args)
       ;; No bindings remaining for src: remove it from dest's body
       (update-in db [:program dest-rule-idx :body]
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
       (let [updated-program
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
              (:program db))]
         (-> db
             (assoc :program updated-program)
             (dissoc :connecting-dest)
             (dissoc :mouse-position)))))))

(re-frame/reg-event-db
 ::mouse-move
 (fn [db [_ mouse-event]]
   (assoc db :mouse-position {:x (.-clientX mouse-event)
                              :y (.-clientY mouse-event)})))
