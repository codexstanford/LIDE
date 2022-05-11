(ns lide.views
  (:require
   [clojure.string :as string]
   [re-frame.core :as re-frame]
   [lide.epilog :as epilog]
   [lide.events :as events]
   [lide.subs :as subs]
   [lide.util :as util]
   ))

(defn center-position [{:keys [position size]}]
  {:x (+ (:x position) (/ (:width size) 2))
   :y (+ (:y position) (/ (:height size) 2))})

(defn matches? [clause matcher])

(defn find-substitutions [program]
  "Infers connections between fully or partially ground rules in `program` and
  the more general rules they can substitute for.

  For example, pred(a, b) could substitute for pred(X, Y). This will not be
  explicitly indicated in `program`, but displaying the semantic connection
  between pred(a, b) and pred(X, Y) is very informative.

  Essentially, we're performing one round of bottom-up unification."
  )

(defn populate-rule [program rule]
  (-> rule
      (update :head (fn [id]
                      (-> program :literals (get id))))
      (update :body (fn [literals]
                      (map (fn [id] (-> program :literals (get id)))
                           literals)))))

(defn rules-view-model [program highlighted-connection]
  ;; TODO support disjunction - we might already have a definition
  (mapv
   (fn [ref-rule]
     (let [rule (populate-rule program ref-rule)
           internals (->> rule
                          :body
                          (mapcat :args)
                          (remove #(= % :unspecified))
                          (remove (fn [internal]
                                    (some #(= internal %) (-> rule :head :args))))
                          vec)]
       {:head (:head rule)
        :internals internals
        :highlight (filterv (fn [arg]
                              (some #(= % [(-> rule :head :predicate) arg])
                                    (vals (select-keys highlighted-connection [:src :dest]))))
                            (concat (-> rule :head :args) internals))}))
   (:rules program)))

(defn literals-view-model [program]
  (->> (:rules program)
       (mapv
        (fn [ref-rule]
          (mapv
           (fn [body-literal-id]
             (let [body-literal (get (:literals program) body-literal-id)]
               {body-literal-id {:predicate (:predicate body-literal)
                                 :args      (:args body-literal)}}))
           (:body ref-rule))))
       flatten
       (into {})))

(defn filter-by-head [head-literal program]
  (->> program
       (filter (fn [[id rule]]
                 (= (-> rule :head :predicate)
                    (-> head-literal :predicate))))
       ;; TODO filter by args as well?
       (into {})))

(defn connections-view-model [program highlighted-connection]
  (->> (:rules program)
       (map-indexed
        (fn [rule-idx rule]
          (map
           (fn [body-literal-id]
             (let [body-literal (get (:literals program) body-literal-id)]
               (map-indexed
                (fn [i arg]
                  (when (not= arg :unspecified)
                    (let [unhighlighted {:src  [rule-idx        arg]
                                         :dest [body-literal-id arg]}]
                      (assoc unhighlighted
                             :highlighted
                             (= unhighlighted highlighted-connection)))))
                (:args body-literal))))
           (:body rule))))
       flatten
       (remove nil?)))

(def rule-head-font-size 18)
(def rule-head-padding 6)
(def rule-binding-font-size 16)
(def rule-binding-padding-x 6)
(def rule-binding-padding-y 3)

(defn literal-layout [literal position]
  (let [name-height (+ rule-head-padding
                       rule-head-font-size
                       rule-head-padding)

        predicate {:predicate (:predicate literal)
                   :position {:x rule-head-padding
                              :y (/ name-height 2)}
                   :size     {:width  150
                              :height name-height}}

        arg-height (+  rule-binding-padding-y
                       rule-binding-font-size
                       rule-binding-padding-y)
        args-y-start (+ name-height
                        (/ arg-height 2))
        args (map-indexed
              (fn [i arg]
                [arg
                 {:position {:y (+ args-y-start
                                   (* i arg-height))}}])
              (:args literal))
        args-height (* arg-height (-> literal :args count))]
    {:predicate predicate
     :args (into {} args)
     :container {:position (or position {:x 0 :y 0})
                 :size {:width  150
                        :height (+ name-height
                                   args-height)}}}))

(defn literal [{:keys [id model layout]}]
  [:g {:on-mouse-down #(re-frame/dispatch [::events/start-drag-literal % id])
       :on-click #(re-frame/dispatch [::events/select-literal id])
       :transform (str "translate(" (-> layout :container :position :x) "," (-> layout :container :position :y) ")")
       :key id}
   [:rect {:class  "rule__bg"
           :width  (->> layout :container :size :width)
           :height (->> layout :container :size :height)}]
   [:text {:x rule-head-padding
           :y (/ (+ rule-head-padding rule-head-font-size rule-head-padding) 2)}
    (:predicate model)]
   [:<>
    (map-indexed
     (fn [arg-index [arg arg-layout]]
       [:text {:x rule-binding-padding-x
               :y (->> arg-layout :position :y)
               :class (when (some #(= % arg) (:highlight model)) "rule--highlight")
               :key arg
               :on-click #(re-frame/dispatch [::events/connect-src-literal id [arg-index arg]])}
        arg])
     (:args layout))]])

(defn rule-layout [rule position]
  (let [name-height (+ rule-head-padding
                       rule-head-font-size
                       rule-head-padding)

        predicate {:predicate (-> rule :head :predicate)
                   :position {:x rule-head-padding
                              :y (/ name-height 2)}
                   :size     {:width  150
                              :height name-height}}

        arg-height (+  rule-binding-padding-y
                       rule-binding-font-size
                       rule-binding-padding-y)
        args-y-start (+ name-height
                        (/ arg-height 2))
        args (map-indexed
              (fn [i arg]
                [arg
                 {:position {:y (+ args-y-start
                                   (* i arg-height))}}])
              (-> rule :head :args))
        args-height (* arg-height (-> rule :head :args count))

        internals-y-start (+ name-height
                             args-height
                             (/ arg-height 2))
        internals (map-indexed
                   (fn [i internal]
                     [internal
                      {:position {:y (+ internals-y-start
                                        (* i arg-height))}}])
                   (:internals rule))
        internals-height (* arg-height (->> rule :internals count))

        add-binding {:position {:y (+ name-height
                                      args-height
                                      internals-height
                                      (/ arg-height 2))}}]
    {:predicate predicate
     :args (into {} args)
     :internals (into {} internals)
     :add-binding add-binding
     :container {:position (or position {:x 0 :y 0})
                 :size {:width  150
                        :height (+ name-height
                                   args-height
                                   internals-height
                                   ;; Add Binding is one arg tall
                                   arg-height)}}}))

(defn rule [{:keys [index model layout]}]
  [:g {:on-mouse-down #(re-frame/dispatch [::events/start-drag-rule % index])
       :on-click #(re-frame/dispatch [::events/select-rule index])
       :transform (str "translate(" (-> layout :container :position :x) "," (-> layout :container :position :y) ")")
       :key index}
   [:rect {:class  "rule__bg"
           :width  (->> layout :container :size :width)
           :height (->> layout :container :size :height)}]
   [:text {:x rule-head-padding
           :y (/ (+ rule-head-padding rule-head-font-size rule-head-padding) 2)}
    (-> model :head :predicate)]
   [:<>
    (map-indexed
     (fn [arg-index [arg arg-layout]]
       [:text {:x rule-binding-padding-x
               :y (->> arg-layout :position :y)
               :class (when (some #(= % arg) (:highlight model)) "rule--highlight")
               :key arg
               :on-click #(re-frame/dispatch [::events/connect-src index [arg-index arg]])}
        arg])
     (concat (:args layout) (:internals layout)))]
   [:text {:x rule-binding-padding-x
           :y (->> layout :add-binding :position :y)
           :on-click #(re-frame/dispatch [::events/start-connect-dest (-> model :head :predicate)])}
    "+ Add Binding"]])

(defn socket-position [layout arg {:keys [end]}]
  (let [all-names (merge (:args layout)
                         (:internals layout))]
    {:x (+ (-> layout :container :position :x)
           (if (= end :dest)
             (-> layout :container :size :width)
             0))
     :y (+ (-> layout :container :position :y)
           (-> all-names (get arg) :position :y))}))

(defn connection [{:keys [connection literal-layouts rule-layouts]}]
  (let [[start-rule-idx start-arg] (:src connection)
        [end-literal-id end-arg]   (:dest connection)
        start-layout (get rule-layouts start-rule-idx)
        end-layout   (get literal-layouts end-literal-id)
        start (socket-position start-layout start-arg {:end :dest})
        end   (socket-position end-layout end-arg {:end :src})]
    [:<>
     [:line {:x1 (:x start)
             :y1 (:y start)
             :x2 (:x end)
             :y2 (:y end)
             :stroke (if (:highlighted connection) "green" "black")}]
     [:line {:x1 (:x start)
             :y1 (:y start)
             :x2 (:x end)
             :y2 (:y end)
             :stroke "transparent"
             :stroke-width 10
             :on-mouse-over #(re-frame/dispatch [::events/highlight-connection (select-keys connection [:src :dest])])
             :on-mouse-leave #(re-frame/dispatch [::events/stop-connection-highlight])
             :on-click #(re-frame/dispatch [::events/disconnect connection])}]]))

(defn rule-inspector [id rule]
  (if-not rule
    [:div "No active selection."]
    [:input {:type "text"
             :value (-> rule :head :predicate)
             :on-change #(re-frame/dispatch [::events/edit-predicate id (-> % .-target .-value)])}]))

(defn main-panel []
  (let [program (re-frame/subscribe [::subs/program])
        rule-positions (re-frame/subscribe [::subs/rule-positions])
        literal-positions (re-frame/subscribe [::subs/literal-positions])
        selected-rule (re-frame/subscribe [::subs/selected-rule])
        connecting-dest (re-frame/subscribe [::subs/connecting-dest])
        mouse-position (re-frame/subscribe [::subs/mouse-position])
        highlighted-connection (re-frame/subscribe [::subs/highlighted-connection])
        graph-transform (re-frame/subscribe [::subs/graph-transform])
        literals-vm (literals-view-model @program)
        literal-layouts (->> literals-vm
                             (map
                              (fn [[id literal]]
                                [id (literal-layout literal (get @literal-positions id))]))
                             (into {}))
        rules-vm (rules-view-model @program @highlighted-connection)
        rule-layouts (->> rules-vm
                          (map-indexed
                           (fn [idx rule]
                             [idx (rule-layout rule (get @rule-positions idx))]))
                          (into {}))
        connections-vm (connections-view-model @program @highlighted-connection)]
    (when @program
      [:div {:id "app-container"
             :on-mouse-up #(re-frame/dispatch [::events/mouse-up %])}
       [:svg {:class "graph-panel"
              :height 500
              :width  1000
              :on-mouse-move (goog.functions.throttle #(re-frame/dispatch [::events/mouse-move %])
                                                      25)
              :on-wheel (goog.functions.throttle #(re-frame/dispatch [::events/scroll-graph %])
                                                 100)}
        [:rect {:class "graph__bg"
                :height 500
                :width  1000
                :on-mouse-down #(re-frame/dispatch [::events/start-drag-graph %])
                :on-click #(re-frame/dispatch [::events/create-node %])}]
        [:g {:class "graph__viewport"
             :transform (when @graph-transform
                          (str (util/dom-matrix-from-vals @graph-transform)))}
         (when (and (not (string/blank? @connecting-dest))
                    @mouse-position)
           (let [origin-center (center-position (get rule-layouts @connecting-dest))]
             [:line {:x1 (:x origin-center)
                     :y1 (:y origin-center)
                     :x2 (:x @mouse-position)
                     :y2 (:y @mouse-position)
                     :stroke "#333"}]))
         (map-indexed (fn [idx rule-vm]
                        [rule {:index  idx
                               :key    idx
                               :model  rule-vm
                               :layout (get rule-layouts idx)}])
              rules-vm)
         (doall
          (map (fn [[id vm]]
                 [literal {:id id
                           :key id
                           :model vm
                           :layout (get literal-layouts id {:x 0 :y 0})}])
               literals-vm))
         (map (fn [vm]
                [connection {:connection vm
                             :literal-layouts literal-layouts
                             :rule-layouts rule-layouts
                             :key vm}])
              connections-vm)]]
       [:div {:class "inspector-panel"}
        (rule-inspector @selected-rule (get @program @selected-rule))]
       [:div {:class "code-panel"}
        [:pre {:class "code"}
         (string/join "\n\n"
                      (map epilog/rule-to-epilog (vals @program)))]]])))
