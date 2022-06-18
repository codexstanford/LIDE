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

;; Layout information
;;
;; For some components of the graph, rather than describing their layout ahead
;; of time and rendering them with pure SVG, we render them into foreignObjects
;; and then read back the resulting layout. These are the functions that extract
;; layout information from rendered components.
;;
;; See subs.cljs for a description of the rendering process.

(defn rule-layout [position element]
  (let [root-position {:x (.-offsetLeft element)
                       :y (.-offsetTop  element)}]
    {:container {:position position
                 :size {:width  (.-offsetWidth  element)
                        :height (.-offsetHeight element)}}
     :body (->> (.querySelectorAll element ".body-literal")
                (map
                 (fn [literal-elem]
                   [(uuid (.getAttribute literal-elem "data-literal-id"))
                    ;; We want the relative position of the attribute row,
                    ;; so subtract the position of the root element.
                    {:position {:x (- (.-offsetLeft literal-elem) (:x root-position))
                                :y (- (.-offsetTop  literal-elem) (:y root-position))}
                     :size {:width  (.-offsetWidth  literal-elem)
                            :height (.-offsetHeight literal-elem)}}]))
                (into {}))}))

(defn fact-layout [position element]
  (let [root-position {:x (.-offsetLeft element)
                       :y (.-offsetTop  element)}]
    {:container {:position position
                 :size {:width  (.-offsetWidth  element)
                        :height (.-offsetHeight element)}}
     :attributes (->> (.querySelectorAll element ".fact__attribute")
                      (map
                       (fn [attr-elem]
                         [(.getAttribute attr-elem "data-attribute-name")
                          {:position {:x (- (.-offsetLeft attr-elem) (:x root-position))
                                      :y (- (.-offsetTop  attr-elem) (:y root-position))}
                           :size {:width  (.-offsetWidth  attr-elem)
                                  :height (.-offsetHeight attr-elem)}}]))
                      (into {}))}))
