(ns lide.epilog
  "Convert logic program constructs to their Epilog representations."
  (:require
   [clojure.string :as string]))

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
                           (if (= arg :unspecified)
                             [(inc anon-idx) (conj acc (str "_" anon-idx))]
                             [anon-idx (conj acc arg)]))
                         [0 []]
                         (:args literal))
                        (get 1)))
          ")"))))

(defn rule-to-epilog [rule defeating-rules]
  (let [head (literal-to-epilog (:head rule))
        body (when (seq (:body rule))
               (string/join " &\n  " (map literal-to-epilog (:body rule))))
        defeaters (when (seq defeating-rules)
                    (string/join " &\n  ~" (map #(literal-to-epilog (:head %)) defeating-rules)))]
    (str head
         (when (or body (seq defeaters))
           (str " :-\n  "))
         body
         (when (and body (seq defeaters))
           (str " &\n  ~"))
         defeaters)))
