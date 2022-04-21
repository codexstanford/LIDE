(ns lide.epilog
  (:require
   [clojure.string :as string]))

(defn literal-to-epilog [literal]
  (str (:predicate literal)
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
       ")"))

(defn rule-to-epilog [rule]
  (let [head (literal-to-epilog (:head rule))
        body (when (seq (:body rule))
               (string/join " &\n  " (map literal-to-epilog (:body rule))))]
    (str head
         (when body
           (str " :-\n  " body)))))
