(ns lide.util
  (:require
   [clojure.string :as string]
   [reagent.core :as r]))

;; General utilities

(defn map-vals [f m]
  (into (empty m) (for [[k v] m] [k (f v)])))

(defn first-indexed [pred coll]
  "Return a vector of the first element in `coll` matching `pred` and the index
  of that element, ordered [index, item]."
  (->> coll
       (keep-indexed (fn [idx elem]
                       (when (pred elem)
                         [idx elem])))
       first))

(defn vector-remove [vec idx]
  (into (subvec vec 0 idx) (subvec vec (inc idx))))

;; DOM transform matrix stuff

(defn dom-matrix-to-vals [dm]
  [(.-a dm)
   (.-b dm)
   (.-c dm)
   (.-d dm)
   (.-e dm)
   (.-f dm)])

(defn dom-matrix-from-vals [vals]
  (js/DOMMatrix. vals))

;; Rule/program utilities

(defn populate-rule [program rule]
  (-> rule
      (update :head (fn [id]
                      (-> program :literals (get id))))
      (update :body (fn [literals]
                      (mapv (fn [id] (-> program :literals (get id)))
                            literals)))))

(defn variable? [arg]
  "True if `arg` is a string starting with an upper-case letter."
  (let [first-char (subs arg 0 1)
        first-char-alpha (apply str (re-seq #"[a-zA-Z]" first-char))]
    (= first-char
       first-char-alpha
       (string/upper-case first-char))))

(defn ground? [arg]
  (not (variable? arg)))

(defn matches? [a b]
  "Literals `a` and `b` match if they have the same predicate, same arity, and
  any ground arguments are the same."
  (and (= (:predicate a)
          (:predicate b))
       (= (count (:args a))
          (count (:args b)))
       (not-any? (fn [[a-arg b-arg]]
                   (and (not= a-arg b-arg)
                        (ground? a-arg)
                        (ground? b-arg)))
                 (map vector (:args a) (:args b)))))

(defn grounds? [a b]
  "Literal `b` grounds literal `a` if they have the same predicate, same arity,
  and at least one shared argument is ground in `b` and variable in `a`. "
  (and (= (:predicate a)
          (:predicate b))
       (= (count (:args a))
          (count (:args b)))
       (some (fn [[a-arg b-arg]]
               (and (variable? a-arg)
                    (ground? b-arg)))
             (map vector (:args a) (:args b)))))

(defn internal-names [rule]
  (->> rule
       :body
       (mapcat :args)
       distinct ;; internals share a namespace
       (remove #(= % :unspecified))
       (remove ground?)
       (remove (fn [internal]
                 (some #(= internal %) (-> rule :head :args))))
       vec))

(defn all-body-literals [program]
  (->> (:rules program)
       (map
        (fn [ref-rule]
          (map
           (fn [body-literal-id]
             {body-literal-id (get (:literals program) body-literal-id)})
           (:body ref-rule))))
       flatten
       (into {})))

(defn compositions [program rule]
  (->> (:body rule)
       (map
        (fn [body-literal-id]
          (let [body-literal (get (:literals program) body-literal-id)]
            (->> (:args body-literal)
                 (mapv
                  (fn [arg]
                    (when (and (not= arg :unspecified)
                               ;; Ground args aren't relevant here because there's
                               ;; no unification to be done with them
                               (not (ground? arg)))
                      {:literal-id body-literal-id
                       :arg        arg})))
                 ((fn [comps]
                    ;; We want there to always be at least one composition returned, so
                    ;; that we can draw at least one edge. When arg is :unbound, we'll just
                    ;; draw a line to the entire literal.
                    (if (empty? comps)
                      [{:literal-id body-literal-id
                        :arg        :unbound}]
                      comps)))))))
       flatten
       (remove nil?)))

(defn remove-literal [program literal-id]
  (-> program
      ;; Remove the literal itself
      (update :literals #(dissoc % literal-id))
      ;; Remove references to the literal in rules
      (update :rules
              (fn [rules]
                (mapv
                 (fn [rule]
                   (update rule
                           :body
                           (fn [body]
                             (vec (remove #(= literal-id %) body)))))
                 rules)))))

;; Reusable components

(defn eip-svg-text-input [{:keys [value on-blur stop-editing x y width height]}]
  (let [!value (r/atom value)]
    (fn [{:keys [value on-blur x y width height]
          :or {on-change (fn [] nil)
               on-blur   (fn [] nil)}}]
      (let [blur #(do (stop-editing)
                      (on-blur %))]
        [:foreignObject {:x 0
                         :y (- y (/ height 2))
                         :width width
                         :height height}
         [:input {:ref #(when % (do (.focus %)
                                    (.select %)))
                  :class "eip-svg-text__input"
                  :value @!value
                  :on-change #(reset! !value (-> % .-target .-value))
                  :on-blur #(blur %)
                  ;; Notice that the event blur is passed here is not in fact a
                  ;; blur event. This could be misleading to callers but
                  ;; event.target.value still works and that's all I use it for
                  ;; for now
                  :on-key-down #(when (contains? #{"Enter" "Escape"} (.-key %))
                                  (blur %))}]]))))

(defn eip-svg-text [{:keys [value x y display-class]}]
  (let [!editing? (r/atom false)
        start-editing #(reset! !editing? true)
        stop-editing  #(reset! !editing? false)]
    (fn [{:keys [value x y display-class] :as props}]
      (if @!editing?
        [eip-svg-text-input (assoc props :stop-editing stop-editing)]
        [:text {:class display-class
                :x x
                :y y
                :on-click start-editing}
         value]))))
