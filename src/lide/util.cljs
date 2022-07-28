(ns lide.util
  (:require
   [clojure.string :as string]
   [reagent.core :as r]))

;; General utilities

(defn map-keys
  "Apply `f` to keys in `m`, keeping the same values."
  [f m]
  (->> m
       (map
        (fn [[k v]]
          [(f k) v]))
       (into {})))

(defn map-vals
  "Apply `f` to values in `m`, keeping the same keys."
  [f m]
  (into (empty m) (for [[k v] m] [k (f v)])))

(defn first-indexed
  "Return a vector of the first element in `coll` matching `pred` and the index
  of that element, ordered [index, item]."
  [pred coll]
  (->> coll
       (keep-indexed (fn [idx elem]
                       (when (pred elem)
                         [idx elem])))
       first))

(defn vector-remove
  "Remove the element at `idx` from `vec`."
  [vec idx]
  (into (subvec vec 0 idx) (subvec vec (inc idx))))

(defn hash-to-hsl [s]
  (->> s
       (. js/lide hashString s)
       (#(* % 360))
       (#(. js/Math floor %))
       (#(str "hsl(" % ",100%,33%)"))))

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

(defn populate-rule
  "Retrieve the actual values from `program` pointed to by the IDs in `rule`."
  [program rule]
  (-> rule
      (update :head (fn [id]
                      (-> program :literals (get id))))
      (update :body (fn [literals]
                      (mapv (fn [id]
                              (assoc (-> program :literals (get id))
                                     :id id))
                            literals)))))

(defn variable?
  "True if `arg` is a string starting with an upper-case letter."
  [arg]
  (if (string/blank? arg)
      false
      (let [first-char (subs arg 0 1)
            first-char-alpha (apply str (re-seq #"[a-zA-Z]" first-char))]
        (= first-char
           first-char-alpha
           (string/upper-case first-char)))))

(defn ground? [arg]
  (not (variable? arg)))

(defn matches?
  "Literals `a` and `b` match if they have the same predicate, same arity, and
  any ground arguments are the same."
  [a b]
  (and (= (:predicate a)
          (:predicate b))
       (= (count (:args a))
          (count (:args b)))
       (not-any? (fn [[a-arg b-arg]]
                   (and (not= a-arg b-arg)
                        (ground? a-arg)
                        (ground? b-arg)))
                 (map vector (:args a) (:args b)))))

(defn grounds?
  "Literal `b` grounds literal `a` if they have the same predicate, same arity,
  and at least one shared argument is ground in `b` and variable in `a`. "
  [a b]
  (and (= (:predicate a)
          (:predicate b))
       (= (count (:args a))
          (count (:args b)))
       (some (fn [[a-arg b-arg]]
               (and (variable? a-arg)
                    (ground? b-arg)))
             (map vector (:args a) (:args b)))))

(defn parse-body-arg
  "Parse an argument to a body literal. Some special formats are allowed in this
  context: `arg` might be more than a plain string."
  [arg]
  (let [unparsable {:type :uncontrolled
                    :value arg}]
    (cond
      (string/blank? arg) unparsable ;; redundant with :else but a useful guard

      (> (count (string/split arg "->")) 1)
      {:type :attribute-path
       :value (string/split arg "->")}

      :else unparsable)))

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

(defn find-rule-matches
  "Find head literals that match with body literals from other rules."
  [program]
  (->> (:rules program)
       (map
        (fn [[rule-id rule]]
          (->> (:body rule)
               (mapv
                (fn [body-literal-id]
                  (let [body-literal (get (:literals program) body-literal-id)]
                    (->> (:rules program)
                         (map (fn [[id rule]] [id rule]))
                         (filter (fn [[_ grounding-rule]]
                                   (matches? body-literal (get (:literals program)
                                                               (:head grounding-rule)))))
                         (map (fn [[grounding-rule-id grounding-rule]]
                                {:src  grounding-rule-id
                                 :dest [rule-id body-literal-id]})))))))))
       flatten))

(defn remove-literal [program literal-id]
  (-> program
      ;; Remove the literal itself
      (update :literals #(dissoc % literal-id))
      ;; Remove references to the literal in rules
      (update :rules
              (fn [rules]
                (map-vals
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

(defn eip-svg-text [{:keys [value x y display-style]}]
  (let [!editing? (r/atom false)
        start-editing #(reset! !editing? true)
        stop-editing  #(reset! !editing? false)]
    (fn [{:keys [value x y display-style] :as props}]
      (if @!editing?
        [eip-svg-text-input (assoc props :stop-editing stop-editing)]
        [:text {:style display-style
                :x x
                :y y
                :on-click start-editing}
         value]))))

(defn eip-plain-text [{:keys [value on-blur style]}]
  (let [!value (r/atom value)
        !input (r/atom nil)]
    (fn [{:keys [class on-blur style value]
          :or {style (constantly {})}
          :as props}]
      [:input (merge
               props
               {:ref #(reset! !input %)
                :class (str "eip-plain-text " class)
                :value @!value
                :on-focus #(.select @!input)
                :on-change #(reset! !value (-> % .-target .-value))
                :on-key-down #(when (contains? #{"Enter" "Escape"} (.-key %))
                                (.blur @!input))
                :style (when @!value (style @!value))})])))

(defn style-arg
  "Hash `arg` to a color if it's a variable and return an appropriate style map."
  [arg]
  (if (variable? arg)
    {"color" (hash-to-hsl arg)}
    {}))
