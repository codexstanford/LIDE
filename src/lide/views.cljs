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

(defn localize-event-to-svg [svg event]
  (let [dom-point (js/DOMPoint. (.-clientX event) (.-clientY event))
        svg-point (.matrixTransform dom-point (.inverse (.getScreenCTM ^js svg)))]
    {:x (.-x svg-point)
     :y (.-y svg-point)}))

(defn rule-view-model [rule highlighted-connection]
  (let [internals (->> rule
                       :body
                       (mapcat :args)
                       (remove #(= % :unspecified))
                       (remove util/ground?)
                       (remove (fn [internal]
                                 (some #(= internal %) (-> rule :head :args))))
                       vec)]
    {:head (:head rule)
     :internals internals
     :highlight (filterv (fn [arg]
                           (some #(= % [(-> rule :head :predicate) arg])
                                 (vals (select-keys highlighted-connection [:src :dest]))))
                         (concat (-> rule :head :args) internals))}))

(defn rules-view-model [program highlighted-connection]
  ;; TODO support disjunction - we might already have a definition
  (->> (:rules program)
       (mapv #(util/populate-rule program %))
       (mapv #(rule-view-model % highlighted-connection))))

(defn literal-view-model [literal]
  {:predicate (:predicate literal)
   :args      (:args literal)})

(defn literals-view-model [program]
  (->> (:rules program)
       (mapv
        (fn [ref-rule]
          (mapv
           (fn [body-literal-id]
             (let [body-literal (get (:literals program) body-literal-id)]
               {body-literal-id (literal-view-model body-literal)}))
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

(defn compositions-view-model [program rule-idx rule highlighted-connection]
  (->> rule
       (util/compositions program)
       (mapv
        (fn [composition]
          ;; arg from composition should match an arg in rule, so we can use it on
          ;; both ends of the connector
          (let [arg (:arg composition)]
            {:src [rule-idx arg]
             :dest [(:literal-id composition) arg]})))))

;; We're looking for head literals that ground body literals from other rules.

(defn groundings-view-model [program]
  (->> (:rules program)
       (map-indexed
        (fn [rule-idx rule]
          (->> (:body rule)
               (mapv
                (fn [body-literal-id]
                  (let [body-literal (get (:literals program) body-literal-id)]
                    (->> (:rules program)
                         (map-indexed (fn [idx rule] [idx rule]))
                         (filter (fn [[_ grounding-rule]]
                                   (util/grounds? body-literal (get (:literals program)
                                                                    (:head grounding-rule)))))
                         (map (fn [[grounding-rule-idx grounding-rule]]
                                {:src  [grounding-rule-idx (->> grounding-rule
                                                                :head
                                                                (get (:literals program))
                                                                :args)]
                                 :dest [body-literal-id (:args body-literal)]})))))))))
       flatten))

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

(defn literal [{:keys [id local-position]}]
  (let [literal-raw @(re-frame/subscribe [::subs/literal id])
        literal-model (literal-view-model literal-raw)
        position @(re-frame/subscribe [::subs/literal-position id])
        layout (literal-layout literal-model position)]
    [:g {:on-mouse-down #(re-frame/dispatch [::events/start-drag-literal (local-position %) id])
         :on-click #(re-frame/dispatch [::events/select-literal id])
         :transform (str "translate(" (-> layout :container :position :x) "," (-> layout :container :position :y) ")")
         :key id}
     [:rect {:class  "rule__bg"
             :width  (->> layout :container :size :width)
             :height (->> layout :container :size :height)}]
     [util/eip-svg-text
      {:val (:predicate literal-model)
       :on-change #(re-frame/dispatch-sync [::events/edit-literal-predicate id (-> % .-target .-value)])
       :x rule-head-padding
       :y (/ (+ rule-head-padding rule-head-font-size rule-head-padding) 2)
       :width (->> layout :container :size :width)
       :height (+ rule-head-padding rule-head-font-size rule-head-padding)}]
     [:<>
      (map-indexed
       (fn [arg-index [arg arg-layout]]
         [util/eip-svg-text
          {:val arg
           :on-change #(re-frame/dispatch-sync [::events/edit-literal-arg id arg-index (-> % .-target .-value)])
           :x rule-binding-padding-x
           :y (->> arg-layout :position :y)
           :width  (->> layout :container :size :width)
           :height (+ rule-head-padding rule-head-font-size rule-head-padding)
           :class (when (some #(= % arg) (:highlight literal-model)) "rule--highlight")
           :key arg-index}])
       (:args layout))]]))

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

(defn rule [{:keys [index local-position]}]
  ;; TODO Should get layout data for EIPs from layout object
  (let [rule-raw @(re-frame/subscribe [::subs/populated-rule index])
        highlighted-connection @(re-frame/subscribe [::subs/highlighted-connection])
        rule-model (rule-view-model rule-raw highlighted-connection)
        position @(re-frame/subscribe [::subs/rule-position index])
        layout (rule-layout rule-model position)]
    [:g {:on-mouse-down #(re-frame/dispatch [::events/start-drag-rule (local-position %) index])
         :on-click #(re-frame/dispatch [::events/select-rule index])
         :transform (str "translate(" (-> layout :container :position :x) "," (-> layout :container :position :y) ")")
         :key index}
     [:rect {:class  "rule__bg"
             :width  (->> layout :container :size :width)
             :height (->> layout :container :size :height)}]
     [util/eip-svg-text
      {:val (-> rule-model :head :predicate)
       :on-change #(re-frame/dispatch-sync [::events/edit-head-predicate index (-> % .-target .-value)])
       :x rule-head-padding
       :y (/ (+ rule-head-padding rule-head-font-size rule-head-padding) 2)
       :width  (->> layout :container :size :width)
       :height (+ rule-head-padding rule-head-font-size rule-head-padding)}]
     [:<>
      (map-indexed
       (fn [arg-index [arg arg-layout]]
         [util/eip-svg-text
          {:val arg
           :on-change #(re-frame/dispatch-sync [::events/edit-head-arg index arg-index (-> % .-target .-value)])
           :x rule-binding-padding-x
           :y (->> arg-layout :position :y)
           :width  (->> layout :container :size :width)
           :height (+ rule-head-padding rule-head-font-size rule-head-padding)
           :class (when (some #(= % arg) (:highlight rule-model)) "rule--highlight")
           :key arg-index
           :on-click #(re-frame/dispatch [::events/connect-src index [arg-index arg]])}])
       (concat (:args layout) (:internals layout)))]
     [:text {:x rule-binding-padding-x
             :y (->> layout :add-binding :position :y)
             :on-click #(re-frame/dispatch [::events/add-argument index])}
      "+ Add argument"]]))

(defn socket-position [layout arg {:keys [end]}]
  (let [all-names (merge (:args layout)
                         (:internals layout))]
    {:x (+ (-> layout :container :position :x)
           (if (= end :dest)
             (-> layout :container :size :width)
             0))
     :y (+ (-> layout :container :position :y)
           (-> all-names (get arg) :position :y))}))

(defn composition-connector [connection]
  (let [[start-rule-idx start-arg] (:src connection)
        [end-literal-id end-arg]   (:dest connection)
        start-rule @(re-frame/subscribe [::subs/populated-rule start-rule-idx])
        start-rule-position @(re-frame/subscribe [::subs/rule-position start-rule-idx])
        end-literal @(re-frame/subscribe [::subs/literal end-literal-id])
        end-literal-position @(re-frame/subscribe [::subs/literal-position end-literal-id])
        highlighted-connection @(re-frame/subscribe [::subs/highlighted-connection])
        start-layout (rule-layout (rule-view-model start-rule highlighted-connection)
                                  start-rule-position)
        end-layout   (literal-layout (literal-view-model end-literal)
                                     end-literal-position)
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

(defn composition-connectors [{:keys [rule-index]}]
  (let [program @(re-frame/subscribe [::subs/program])
        rule @(re-frame/subscribe [::subs/rule rule-index])
        compositions-model (compositions-view-model program rule-index rule {})]
    (into [:<>] (mapv (fn [cm]
                        [composition-connector cm])
                      compositions-model))))

(defn grounding-connector [{:keys [connection literal-layouts rule-layouts]}]
  (let [[start-rule-idx start-args] (:src connection)
        [end-literal-id end-args]   (:dest connection)
        start-layout (get rule-layouts start-rule-idx)
        end-layout   (get literal-layouts end-literal-id)]
    (into
     [:<>]
     (mapv
      (fn [start-arg end-arg]
        (let [start (socket-position start-layout start-arg {:end :src})
              end (socket-position end-layout end-arg {:end :dest})]
          [:<>
           [:line {:x1 (:x start)
                   :y1 (:y start)
                   :x2 (:x end)
                   :y2 (:y end)
                   :stroke-dasharray "5,5"
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
      start-args
      end-args))))

;; XXX not used at the moment, rethinking how connections work
#_(defn pending-connector []
  (let [connecting-dest (re-frame/subscribe [::subs/connecting-dest])
        mouse-position (re-frame/subscribe [::subs/mouse-position])]
    (when (and (not (string/blank? @connecting-dest))
               @mouse-position)
      (let [origin-center (center-position (get rule-layouts @connecting-dest))]
        [:line {:x1 (:x origin-center)
                :y1 (:y origin-center)
                :x2 (:x @mouse-position)
                :y2 (:y @mouse-position)
                :stroke "#333"}]))))

(defn graph-viewport [{:keys [set-ref]} & children]
  (let [graph-transform @(re-frame/subscribe [::subs/graph-transform])]
    [:g {:ref set-ref
         :class "graph__viewport"
         :transform (when graph-transform
                      (str (util/dom-matrix-from-vals graph-transform)))}
     children]))

(defn program-graph []
  (let [!svg-viewport (atom nil)]
    (fn []
      (let [local-position (fn [event]
                             (localize-event-to-svg @!svg-viewport event))

            program (re-frame/subscribe [::subs/program])
            rule-positions (re-frame/subscribe [::subs/rule-positions])
            literal-positions (re-frame/subscribe [::subs/literal-positions])
            highlighted-connection (re-frame/subscribe [::subs/highlighted-connection])
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
            groundings-vm (groundings-view-model @program)]
        [:svg {:class "graph-panel"
               :on-mouse-move (goog.functions.throttle #(re-frame/dispatch [::events/mouse-move (local-position %)])
                                                       25)
               :on-mouse-up #(re-frame/dispatch [::events/mouse-up (local-position %)])
               :on-wheel (goog.functions.throttle #(re-frame/dispatch [::events/scroll-graph %])
                                                  100)}
         [:rect {:class "graph__bg"
                 :height 10000
                 :width  10000
                 :on-mouse-down #(re-frame/dispatch [::events/mouse-down-graph-bg (local-position %)])}]
         [graph-viewport
          {:set-ref #(reset! !svg-viewport %)}
          (map-indexed (fn [idx]
                         [rule {:index  idx
                                :local-position local-position
                                :key    idx}])
                       (:rules @program))
          (doall
           (map (fn [[id _]]
                  [literal {:id id
                            :local-position local-position
                            :key id}])
                (util/all-body-literals @program)))
          (map-indexed (fn [rule-idx _]
                         [composition-connectors
                          {:rule-index rule-idx
                           :key rule-idx}])
                       (:rules @program))
          (map (fn [vm]
                 [grounding-connector
                  {:connection vm
                   :literal-layouts literal-layouts
                   :rule-layouts rule-layouts
                   :key (str vm)}])
               groundings-vm)]]))))

(defn epilog-panel []
  (let [rules @(re-frame/subscribe [::subs/populated-rules])]
    [:div {:class "epilog-inspector"}
     [:pre {:class "code"}
      (string/join "\n\n" (map epilog/rule-to-epilog rules))]]))

(defn toolbar []
  (let [undos? @(re-frame/subscribe [:undos?])
        redos? @(re-frame/subscribe [:redos?])]
    [:div {:class "toolbar"}
     [:button {:on-click #(re-frame/dispatch [::events/save])}
      "Save"]
     [:button {:on-click #(re-frame/dispatch [:undo])
               :disabled (not undos?)}
      "Undo"]
     [:button {:on-click #(re-frame/dispatch [:redo])
               :disabled (not redos?)}
      "Redo"]]))

(defn main-panel []
  (let [program (re-frame/subscribe [::subs/program])]
    (when @program
      [:div {:id "app-container"}
       [:div {:class "work-viewport"}
        [program-graph]
        [:div {:class "inspectors"}
         [epilog-panel]]]
       [toolbar]])))
