(ns lide.graph)

;; Layout information
;;
;; For some components of the graph, rather than describing their layout ahead
;; of time and rendering them with pure SVG, we render them into foreignObjects
;; and then read back the resulting layout. These are the functions that extract
;; layout information from rendered components.
;;
;; See subs.cljs for a description of the rendering process.

(defn element-position [element]
  {:x (.-offsetLeft element)
   :y (.-offsetTop  element)})

(defn element-size [element]
  {:width  (.-offsetWidth  element)
   :height (.-offsetHeight element)})

(defn layout-relative-to [root-position element]
  {:size (element-size element)
   :position (merge-with + root-position (element-position element))})

(defn rule-layout [position element]
  (let [socket-elem (.querySelector element ".rule__head-predicate .socket")]
    {:container {:position position
                 :size (element-size element)}
     :socket (layout-relative-to position socket-elem)
     :literals (->> (.querySelectorAll element ".body-literal")
                    (map
                     (fn [literal-elem]
                       (let [src-idx (js/parseInt (.getAttribute literal-elem "data-source-index"))
                             socket-elem (.querySelector literal-elem ".socket")]
                         [src-idx
                          (-> (layout-relative-to position literal-elem)
                              (assoc :socket (layout-relative-to position socket-elem)))])))
                    (into {}))}))

(defn fact-layout [position element]
  (let [root-position (element-position element)]
    {:container {:position position
                 :size (element-size element)}
     :attributes (->> (.querySelectorAll element ".fact__attribute")
                      (map
                       (fn [attr-elem]
                         [(.getAttribute attr-elem "data-attribute-name")
                          {:position (merge-with -
                                                 (element-position attr-elem)
                                                 root-position)
                           :size (element-size attr-elem)}]))
                      (into {}))}))

(defn ys-rule-layout [position element]
  (let [root-position (element-position element)]
    {:container {:position position
                 :size (element-size element)}

     :statements
     (->> (.querySelectorAll element ".ys-statement")
          (map
           (fn [st-elem]
             (let [socket-elem (.querySelector st-elem ".ys-dest-fact .socket")]
               {:position (element-position st-elem)
                :size (element-size st-elem)
                :socket {:position (element-position socket-elem)
                         :size (element-size socket-elem)}
                :facts
                (->> (.querySelectorAll st-elem ".ys-rule-fact")
                     (reduce
                      (fn [acc fact-elem]
                        (let [socket-elem
                              (.querySelector fact-elem ".socket")

                              descriptor
                              (.getAttribute fact-elem "data-fact-descriptor")

                              fact-layout
                              {:position (merge-with -
                                                     (element-position fact-elem)
                                                     root-position)
                               :size (element-size fact-elem)
                               :socket (when socket-elem {:position (element-position socket-elem)
                                                          :size (element-size socket-elem)})}]
                          (assoc acc descriptor (if (contains? acc descriptor)
                                                  (conj (get acc descriptor) fact-layout)
                                                  [fact-layout]))))
                      {}))})))
          vec)}))
