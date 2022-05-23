(ns lide.graph)

(def rule-container-width 150)
(def rule-head-font-size 18)
(def rule-head-padding 6)
(def rule-head-height (+ rule-head-font-size
                         (* 2 rule-head-padding)))
(def rule-binding-font-size 16)
(def rule-binding-padding-x 6)
(def rule-binding-padding-y 3)
(def rule-body-padding-x 5)

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
        args-height (* arg-height (-> literal :args count))

        add-argument {:position {:y (+ name-height
                                       args-height
                                       (/ arg-height 2))}}]
    {:id (:id literal)
     :predicate predicate
     :args (into {} args)
     :add-argument add-argument
     :container {:position (or position {:x 0 :y 0})
                 :size {:width  (- rule-container-width
                                   (* 2 rule-body-padding-x))
                        :height (+ name-height
                                   args-height
                                   arg-height)}}}))

(defn rule-layout [rule]
  (let [name-height (+ rule-head-padding
                       rule-head-font-size
                       rule-head-padding)

        predicate {:predicate (-> rule :head :predicate)
                   :position {:x rule-head-padding
                              :y (/ name-height 2)}
                   :size     {:width  150
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

        body (->> (:body rule)
                  (reduce (fn [[body-layouts position] literal]
                            (let [layout (literal-layout literal position)]
                              [(conj body-layouts layout)
                               (update position :y #(+ % (-> layout :container :size :height)))]))
                          [[] {:x rule-body-padding-x
                               :y (+ name-height
                                     args-height
                                     internals-height
                                     arg-height)}])
                  first)

        body-height (reduce + (map #(-> % :container :size :height) body))

        add-body-literal {:position {:y (+ name-height
                                           args-height
                                           internals-height
                                           arg-height
                                           body-height
                                           (/ arg-height 2))}}]
    {:predicate predicate
     :args (into {} args)
     :internals (into {} internals)
     :add-argument add-argument
     :body body
     :add-body-literal add-body-literal
     :container {:size {:width  150
                        :height (+ name-height
                                   args-height
                                   arg-height
                                   internals-height
                                   arg-height
                                   body-height)}}}))
