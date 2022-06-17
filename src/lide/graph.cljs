(ns lide.graph
  (:require
   [lide.util :as util]))

(def svg-ns "http://www.w3.org/2000/svg")

(def rule-container-min-width 150)
(def rule-head-font-size 18)
(def rule-head-padding 6)
(def rule-head-height (+ rule-head-font-size
                         (* 2 rule-head-padding)))
(def rule-binding-font-size 16)
(def rule-binding-padding-x 6)
(def rule-binding-padding-y 3)
(def rule-body-padding-x 5)
(def rule-body-literal-gutter 5)

(defn get-text-width [font-size s]
  "Get the width of a text element containing `s` rendered into #width-test-svg."
  (let [svg (. js/document getElementById "width-test-svg")]
    (if (nil? svg)
      0
      (let [text-element (. js/document createElementNS svg-ns "text")
            _ (. text-element setAttributeNS nil "font-size" font-size)
            _ (. text-element appendChild (. js/document createTextNode s))
            _ (. svg appendChild text-element)
            width (. text-element getComputedTextLength)
            _ (. text-element remove)]
        width))))

(defn literal-label [literal]
  (str (when (:negative literal) "~")
       (:predicate literal)))

(defn literal-layout-collapsed [min-width position literal]
  (let [height (+ rule-head-font-size (* 2 rule-head-padding))

        label (literal-label literal)
        label-width (get-text-width rule-head-font-size label)

        args (->> (:args literal)
                  (reduce
                   (fn [[arg-layouts position] arg]
                     (let [arg-width (get-text-width rule-head-font-size arg)]
                       [(assoc arg-layouts
                               arg
                               {:position position
                                :size {:height height
                                       :width arg-width}})
                        (update position :x #(+ % arg-width))]))
                   [{} {:x label-width
                        :y (/ height 2)}])
                  first)

        args-width (->> args
                        vals
                        (map #(-> % :size :width))
                        (reduce + 0))]
    {:collapsed true
     :id (:id literal)
     :predicate {:predicate label
                 :size {:width label-width}}
     :args args
     :container {:position position
                 :size {:height height
                        :width  (max min-width
                                     (+ label-width (* 2 rule-binding-padding-x))
                                     (+ label-width args-width))}}}))

(defn literal-layout-uncollapsed [min-width position literal]
  (let [label (literal-label literal)
        label-width (get-text-width rule-head-font-size label)

        name-height (+ rule-head-padding
                       rule-head-font-size
                       rule-head-padding)

        predicate {:predicate label
                   :position {:x rule-head-padding
                              :y (/ name-height 2)}
                   :size     {:width label-width
                              :height name-height}}

        arg-height (+ rule-binding-padding-y
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
        args-height (* arg-height (-> literal :args count))

        add-argument {:position {:y (+ name-height
                                       args-height
                                       (/ arg-height 2))}}]
    {:id (:id literal)
     :predicate predicate
     :args (into {} args)
     :add-argument add-argument
     :container {:position (or position {:x 0 :y 0})
                 :size {:width  (max min-width
                                     (+ label-width (* 2 rule-binding-padding-x)))
                        :height (+ name-height
                                   args-height
                                   arg-height)}}}))

(defn literal-layout [min-width position literal]
  (if (:collapsed literal)
    (literal-layout-collapsed   min-width position literal)
    (literal-layout-uncollapsed min-width position literal)))

(defn rule-layout [rule]
  (let [label (-> rule :head :predicate)
        label-width (get-text-width rule-head-font-size label)

        name-height (+ rule-head-padding
                       rule-head-font-size
                       rule-head-padding)

        predicate {:predicate label
                   :position {:x rule-head-padding
                              :y (/ name-height 2)}
                   :size     {:width  label-width
                              :height name-height}}

        ;; Add Binding is also one arg tall
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
                                        (* i arg-height))}
                       :size {:width 150
                              :height arg-height}}])
                   (:internals rule))
        internals-height (* arg-height (->> rule :internals count))

        add-argument {:position {:y (+ name-height
                                       args-height
                                       internals-height
                                       (/ arg-height 2))}}

        literal-min-width (max label-width
                               (- rule-container-min-width (* 2 rule-binding-padding-x)))
        body-unset-width (->> (:body rule)
                              (reduce (fn [[body-layouts position] literal]
                                        (let [layout (literal-layout literal-min-width position literal)]
                                          [(assoc body-layouts (:id literal) layout)
                                           (update position :y #(+ %
                                                                   (-> layout :container :size :height)
                                                                   rule-body-literal-gutter))]))
                                      [{} {:x rule-body-padding-x
                                           :y (+ name-height
                                                 args-height
                                                 internals-height
                                                 arg-height)}])
                              first)
        max-body-width (or (->> body-unset-width
                                vals
                                (map #(-> % :container :size :width))
                                (apply max))
                           0)
        body (util/map-vals (fn [literal]
                              (assoc-in literal [:container :size :width] max-body-width))
                   body-unset-width)
        body-height (->> body
                         vals
                         (map #(-> % :container :size :height))
                         (interpose rule-body-literal-gutter)
                         (reduce + 0))

        add-body-literal {:position {:y (+ name-height
                                           args-height
                                           internals-height
                                           body-height
                                           arg-height
                                           (/ arg-height 2))}}

        add-defeat {:position {:y (+ name-height
                                     args-height
                                     internals-height
                                     arg-height
                                     arg-height
                                     body-height
                                     (/ arg-height 2))}}]
    {:predicate predicate
     :args (into {} args)
     :internals (into {} internals)
     :add-argument add-argument
     :body body
     :add-body-literal add-body-literal
     :add-defeat add-defeat
     :container {:size {:width  (max rule-container-min-width
                                     (+ label-width (* 2 rule-head-padding))
                                     (+ max-body-width (* 2 rule-head-padding)))
                        :height (+ name-height
                                   args-height
                                   arg-height
                                   internals-height
                                   body-height
                                   arg-height
                                   arg-height)}}}))

(defn fact-layout [position element]
  "Compute useful information about the layout of a rendered fact.

  The fact in question is already rendered as `element`, and we're just reading
  data from there."
  (let [root-position {:x (.-offsetLeft element)
                       :y (.-offsetTop  element)}]
    ;; n.b. Container position isn't decided here, just passed in. Positions are
    ;; figured separately because they're user-writable - here we're mostly just
    ;; figuring out what the rendering engine did for us.
    {:container {:position position
                 :size {:width  (.-offsetWidth  element)
                        :height (.-offsetHeight element)}}
     :attributes (->> (.querySelectorAll element ".fact__attribute")
                      (map
                       (fn [attr-elem]
                         [(.getAttribute attr-elem "data-attribute-name")
                          ;; We want the relative position of the attribute row,
                          ;; so subtract the position of the root element.
                          {:position {:x (- (.-offsetLeft attr-elem) (:x root-position))
                                      :y (- (.-offsetTop  attr-elem) (:y root-position))}
                           :size {:width  (.-offsetWidth  attr-elem)
                                  :height (.-offsetHeight attr-elem)}}]))
                      (into {}))}))
