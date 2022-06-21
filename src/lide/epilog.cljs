(ns lide.epilog
  "Convert logic program constructs to their Epilog representations."
  (:require
   [clojure.string :as string]
   [lide.util :as util]))

(defn literal-to-epilog [literal]
  (str (when (:negative literal) "~")
       (:predicate literal)
       (when (not-empty (:args literal))
         (str
          "("
          (string/join ", "
                       (->
                        (reduce
                         (fn [[anon-idx acc] arg]
                           (cond
                             (= arg :unspecified)
                             [(inc anon-idx) (conj acc (str "_" anon-idx))]

                             (> (count (string/split arg "->")) 1)
                             [anon-idx (conj acc (string/replace arg "->" "_"))]

                             :else
                             [anon-idx (conj acc arg)]))
                         [0 []]
                         (:args literal))
                        (get 1)))
          ")"))))

(defn attribute-var-name [threading-var attr]
  (str threading-var "_" attr))

(defn minimize-attribute-subgoals [threading-var attrs]
  (mapcat (fn [[attr sub-attrs]]
            (concat
             [[threading-var attr]]
             (minimize-attribute-subgoals (attribute-var-name threading-var attr) sub-attrs)))
          attrs))

(defn required-attributes [literals]
  "Determine what attribute accesses are needed to produce the rule body
  constituted by `literals`.

  Some of `literals` may have arguments representing attribute paths, which map
  to more than one subgoal."
  (->> literals
       (map :args)
       flatten
       (map util/parse-body-arg)
       (filter #(= :attribute-path (:type %)))
       (map :value)
       (reduce (fn [acc attr-path]
                 (update-in acc attr-path #(or % {})))
               {})
       (mapcat (fn [[arg attrs]]
                 (minimize-attribute-subgoals arg attrs)))))

(defn attribute-subgoals [attrs]
  (map (fn [[var attr]]
         (str attr "(" var ", " (attribute-var-name var attr) ")"))
       attrs))

(defn rule-to-epilog [rule defeating-rules]
  (let [head (literal-to-epilog (:head rule))
        attrs (string/join " &\n  " (attribute-subgoals (required-attributes (:body rule))))
        body (string/join " &\n  " (map literal-to-epilog (:body rule)))
        defeaters (string/join " &\n  " (map #(str "~" (literal-to-epilog (:head %))) defeating-rules))
        sections (remove string/blank? [attrs body defeaters])]
    (str head
         (when (seq sections)
           (str " :-\n  "))
         (string/join " &\n  " sections))))

(defn attribute-value-to-string [facts {:keys [type value]}]
  (condp = type
    :primitive value
    :subobject (-> facts (get value) :name)))

(defn fact-to-epilog [facts id {:keys [name attributes]}]
  (->> attributes
       (map (fn [[k v]]
              (str k "(" name  ", " (attribute-value-to-string facts v) ")")))
       (string/join "\n")))
