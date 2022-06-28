(ns lide.epilog.core
  "Convert logic program constructs to their Epilog representations."
  (:require
   [clojure.string :as string]
   [lide.util :as util]))

(defn attribute-predicate [attr-name]
  (str "ATTR_" attr-name))

(defn stringify-literal [literal]
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
         {:predicate (attribute-predicate attr)
          :args [var (attribute-var-name var attr)]})
       attrs))

(defn compile-rule [rule defeating-rules]
  (let [attrs (attribute-subgoals (required-attributes (:body rule)))
        body (:body rule)
        defeaters (map #(assoc (:head %) :negative true) defeating-rules)]
    {:head (:head rule)
     :body (concat attrs body defeaters)}))

(defn stringify-rule [compiled-rule]
  (str (stringify-literal (:head compiled-rule))
       (when (seq (:body compiled-rule))
         (str " :-\n  "))
       (string/join " &\n  " (map stringify-literal (:body compiled-rule)))))

(defn stringify-converse-operation [compiled-rule]
  "Render an operation that is the converse of `compiled-rule`.

  These are useful for forward chaining."
  (str "tick"
       (when (seq (:body compiled-rule))
         (str " ::\n  "
              (string/join " &\n  " (map stringify-literal (:body compiled-rule)))))
       "\n  ==> "
       (stringify-literal (:head compiled-rule))))

(defn attribute-value-to-string [facts {:keys [type value]}]
  (condp = type
    :primitive value
    :subobject (-> facts (get value) :type)))

(defn stringify-fact [facts id {:keys [type attributes]}]
  (->> attributes
       (map (fn [[k v]]
              (str (attribute-predicate k) "(" type  ", " (attribute-value-to-string facts v) ")")))
       (string/join "\n")))
