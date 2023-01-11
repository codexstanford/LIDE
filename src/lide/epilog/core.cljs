(ns lide.epilog.core
  "Convert logic program constructs to their Epilog representations."
  (:require
   clojure.set
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

(defn required-attributes
  "Determine what attribute accesses are needed to produce the rule body
  constituted by `literals`.

  Some of `literals` may have arguments representing attribute paths, which map
  to more than one subgoal."
  [literals]
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

(defn stringify-converse-operation
  "Render an operation that is the converse of `compiled-rule`.

  These are useful for forward chaining."
  [compiled-rule]
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

(defn sanitize-positions
  "Make position data writable to .lide/positions.json."
  [positions]
  (update positions :rule #(util/map-keys (fn [key-vec]
                                            (string/join ":" key-vec))
                                          %)))

(defn parse-positions
  "Parse position data as read from .lide/positions.json."
  [^js positions]
  (-> (js->clj positions)
      (clojure.set/rename-keys {"rule" :rule})
      (update :rule #(util/map-keys
                      (fn [key-string]
                        (let [[head idx] (string/split key-string #":")]
                          [head (js/parseInt idx)]))
                      %))
      (update :rule #(util/map-vals
                      (fn [position]
                        {:x (get position "x")
                         :y (get position "y")})
                      %))))

(defn variable? [arg]
  (= "variable" (:type arg)))

(defn attribute-access? [literal]
  ;; TODO server could check metadata to decide whether predicate is an
  ;; attribute accessor more robustly
  (and (= 2 (count (clojure.string/split (-> literal :predicate :text) #"\.")))
       (= 2 (count (:args literal)))
       (every? variable? (:args literal))))

(defn expand-attr-path [provenance path]
  (let [[parent-variable attr-name] (get provenance (first path))]
    (if parent-variable
      (expand-attr-path provenance
                        (concat [parent-variable attr-name] (drop 1 path)))
      path)))

(defn to-attr-path [provenance arg]
  (if (not (variable? arg))
    (:text arg)
    (->> (:text arg)
         vector
         (expand-attr-path provenance)
         (clojure.string/join "🡒"))))

(defn condense-attributes
  "Return the body of `rule` with attribute access literals condensed.

  'Condensed' means that (1) when a body literal binds a variable that derives
  completely and uniquely from attribute accesses, that variable's provenance is
  presented inline and (2) body literals that are used only to access such
  attributes are removed when their existence can instead be implied.

  For example:

  p(A, B) :-
    a.x(A, X) &
    q(X) &
    x.y(X, Y) &
    r(Y)

  would yield

  p(A) :-
    q(A->x) &
    r(A->x->y)"
  [rule]
  (let [used
        (->> (:body rule)
             (reduce
              (fn [acc literal]
                (if (attribute-access? literal)
                  ;; Only the first arg, the entity having an attribute accessed,
                  ;; is considered "used", i.e. simply being accessed does not
                  ;; constitute use of an attribute
                  (assoc acc (-> (:args literal) first :text) true)
                  ;; For other kinds of literals, all args are considered used
                  (->> (:args literal)
                       (map :text)
                       (map #(vector % true))
                       (into acc))))
              {}))

        ;; `provenance` is a map of variables with unique origins as accessed
        ;; attributes to those origins, e.g.
        ;;
        ;; p(A) :-
        ;;  a.b(A, B) &
        ;;  b.c(B, C)
        ;;
        ;; yields
        ;;
        ;; {"B" ["A" "b"]
        ;;  "C" ["B" "c"]}
        provenance
        (->> (:body rule)
             (reduce
              (fn [acc literal]
                (if (attribute-access? literal)
                  (let [attr-name (subs (re-find #"\..+" (-> literal :predicate :text)) 1)]
                    (update acc
                            (-> (:args literal) second :text)
                            #(clojure.set/union % #{[(-> (:args literal) first :text)
                                                     attr-name]})))
                  acc))
              {})
             (filter
              (fn [[variable provenances]]
                (= 1 (count provenances))))
             (map
              (fn [[variable provenances]]
                [variable (first provenances)]))
             (into {}))]
    (->> (:body rule)
         (map
          (fn [literal]
            ;; An attribute access can be elided where the accessed value is
            ;; used and there is a unique provenance.
            (if (and (attribute-access? literal)
                     (get used (-> (:args literal) second :text))
                     (get provenance (-> (:args literal) second :text)))
              nil
              ;; For other literals, let's see if we can replace some args with
              ;; attribute paths.
              (update literal
                      :args
                      #(map (fn [arg]
                              (assoc arg :text (to-attr-path provenance arg)))
                            %)))))
         (map-indexed
          (fn [idx literal]
            (when literal [idx literal])))
         (remove nil?)
         (into {}))))
