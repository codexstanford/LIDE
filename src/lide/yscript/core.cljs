(ns lide.yscript.core
  (:require
   [clojure.set :as set]
   [clojure.string :as string]
   [instaparse.core :as insta :refer-macros [defparser]]
   [lide.yscript.grammar :as grammar]))

(defn default-fact []
  {:type :boolean
   :descriptor :unspecified
   :value :unknown})

(defn facts-required-by-expression
  "Return a set of all facts required by `expr`."
  [expr]
  (cond
    (= :unspecified expr) #{}
    (uuid? expr) #{expr} ;; `expr` is just a fact
    (= :and (:type expr)) (apply set/union (map facts-required-by-expression (:exprs expr)))
    (= :or  (:type expr)) (apply set/union (map facts-required-by-expression (:exprs expr)))))

(defn facts-required-by-statement
  "Return a set of all facts required for the execution of `rule`."
  [statement]
  (case (:type statement)
    :only-if (facts-required-by-expression (:src-expr statement))))

(defn facts-determined-by-statement
  "Return a set of all facts that can be determined by executing `statement`."
  [statement]
  (case (:type statement)
    :only-if (facts-required-by-expression (:dest-fact statement))))

(defn orphan-facts
  "Return a set of IDs of all facts neither determined nor required by any rule in
  `program`."
  [program]
  (->> (:rules program)
       (reduce
        (fn [orphans [_ rule]]
          (reduce
           (fn [orphans' statement-id]
             (let [statement (get-in program [:statements statement-id])]
               (set/difference orphans'
                               (facts-determined-by-statement statement)
                               (facts-required-by-statement   statement))))
           orphans
           (:statements rule)))
        (set (keys (:facts program))))))

(defn facts-by-descriptor
  "Return a map of descriptor to ID for all facts in `program`."
  [program]
  (->> (:facts program)
       (reduce
        (fn [acc [id fact]]
          (assoc acc (:descriptor fact) id))
        {})))

(defn statements-by-required-fact
  "Return a map of fact ID to a set of IDs of all statements requiring that fact."
  [program]
  (->> (:statements program)
       (reduce
        (fn [acc [st-id statement]]
          (->> (facts-required-by-statement statement)
               (map #(vector % #{st-id}))
               (into {})
               (merge-with set/union acc)))
        {})))

(defn statements-by-determined-fact
  "Return a map from fact ID to set of IDs of statements that can determine a value
  for that fact."
  [program]
  (->> (:statements program)
       (reduce
        (fn [determiners [st-id statement]]
          (->> (facts-determined-by-statement statement)
               (map #(vector % #{st-id}))
               #_(mapcat
                (fn [[st-id facts]]
                  (map #(vector % #{st-id}) facts)))
               (into {})
               (merge-with set/union determiners)))
        {})))

(defn rules-by-statement
  "Return a map from statement ID to ID of the rule containing that statement."
  [program]
  (->> (:rules program)
       (reduce
        (fn [acc [rule-id rule]]
          (->> (:statements rule)
               (map
                (fn [st-id]
                  [st-id rule-id]))
               (into acc)))
        {})))

(defn compute-expression
  "Compute the value of `expression` given the information in `program`.

  It may not be possible to determine a value, in which case `:unknown` is
  returned.

  TODO this would memoize very nicely"
  [program expr]
  (cond
    (= :unspecified expr)
    :unknown

    (uuid? expr)
    (get-in program [:facts expr :value])

    (= :and (:type expr))
    (cond
      (some #(= false (compute-expression program %)) (:exprs expr)) false
      (every? #(= true (compute-expression program %)) (:exprs expr)) true
      :else :unknown)

    (= :or (:type expr))
    (cond
      (some #(= true (compute-expression program %)) (:exprs expr)) true
      (every? #(= false (compute-expression program %)) (:exprs expr)) false
      :else :unknown)))

(defn compute-statement
  "Determine whatever fact values `statement` can, and return a map of such fact
  IDs to their determined values."
  [program statement]
  (case (:type statement)
    :only-if {(:dest-fact statement) (compute-expression program (:src-expr statement))}))

(defn execute-statement
  "Determine whatever fact values `statement` can, and apply these changes to
  `program`."
  [program statement]
  (case (:type statement)
    :only-if (->> (compute-statement program statement)
                  (reduce
                   (fn [program' [fact-id value]]
                     (assoc-in program' [:facts fact-id :value] value))
                   program))))

(defn forward-chain
  "Infer as much as possible from fact `fact-id` and return `program` with new
  conclusions added."
  [program index fact-id]
  (->> (get-in index [:statements-by-required-fact fact-id])
       ;; TODO should handle rule order
       (reduce (fn [program' statement-id]
                 (let [statement (get-in program [:statements statement-id])
                       program'' (execute-statement program' statement)]
                   (->> statement
                        facts-determined-by-statement
                        (reduce (fn [program''' determined-fact]
                                  (forward-chain program''' index determined-fact))
                                program''))))
               program)))

(defn codify-fact-reference [fact]
  (if (= :unspecified (:descriptor fact))
    "[not set]"
    (:descriptor fact)))

(defn codify-expression [expr]
  (cond
    (= :boolean (:type expr))
    (codify-fact-reference expr)

    (= :and (:type expr))
    (->> (:exprs expr)
         (map codify-expression)
         (string/join " AND\n    "))

    (= :or (:type expr))
    (->> (:exprs expr)
         (map codify-expression)
         (string/join " OR\n    "))))

(defn codify-statement [statement]
  (case (:type statement)
    :only-if
    (str
     "  " (codify-fact-reference (:dest-fact statement)) " ONLY IF\n"
     "    " (codify-expression (:src-expr statement)))))

(defn codify-rule [rule]
  (str
   (string/join " " (remove string/blank? ["RULE" (:name rule) "PROVIDES"])) "\n"
   (string/join "\n" (map codify-statement (vals (:statements rule))))))

(defn codify-program [program]
  (string/join "\n\n" (map codify-rule (vals (:rules program)))))

(defparser parse
  "
code = block+
keyword = 'ALIAS' | 'AS' | 'BOOLEAN' | 'DATE' | 'DISPLAYED' | 'ELSE' | 'EXIT' | 'FORWARD' | 'GREATER' | 'INCLUDE' | 'LESSEQUAL' | 'MATCHES' | 'MONTH' | 'NUMBERED' | 'PERSON' | 'PROVIDES' | 'SECOND' | 'STYLE' | 'THAN' | 'TRANSLATE' | 'UNREPORTED' | 'WHEN' | 'ALL' | 'ASSERT' | 'BY' | 'DAY' | 'DIVIDED' | 'END' | 'EXPLAIN' | 'FROM' | 'GREATEREQUAL' | 'INFO' | 'LEVEL' | 'MINUS' | 'MONTHS' | 'ONLY' | 'PERSON-THING' | 'RANGE' | 'SECONDS' | 'SUB-RULE' | 'THEN' | 'UNCERTAIN' | 'UNTIL' | 'WHILE' | 'AND' | 'ATTACH' | 'CALL' | 'DAYS' | 'DO' | 'END-SECTION' | 'FACT' | 'GENDER' | 'HOUR' | 'INFORMAL' | 'LINE' | 'MINUTE' | 'NEXT' | 'OR' | 'PERSONTHING' | 'REPEAT' | 'SECTION' | 'SUBRULE' | 'THING' | 'UNIT' | 'VERB' | 'YEAR' | 'AND/OR' | 'AUTHORITY' | 'CASE' | 'DEFAULT' | 'DOCUMENT' | 'EQUAL' | 'FOR' | 'GENDER-NEUTRAL' | 'HOURS' | 'INTEGER' | 'LINK' | 'MINUTES' | 'NOT' | 'OR/WITH' | 'PLUS' | 'REPORT' | 'SESSION' | 'SYSTEM' | 'TIME' | 'UNKNOWN' | 'VERBS' | 'YEARS' | 'AND/OR/WITH' | 'BACKWARD' | 'CONTEXT' | 'DEFINITE' | 'DOLLAR' | 'EQUALS' | 'FORGET' | 'GENDERNEUTRAL' | 'IF' | 'IS' | 'LISTED' | 'MOD' | 'NUMBER' | 'ORDER' | 'PROCEDURE' | 'RULE' | 'SEX' | 'TEMPLATE' | 'TIMES' | 'UNLISTED' | 'WEEK' | 'AND/WITH' | 'BEGIN' | 'DAEMON' | 'DETERMINE' | 'DOLLARS' | 'EXAMPLE' | 'FORMAL' | 'GOAL' | 'IN' | 'LESS' | 'MATCH' | 'MONEY' | 'NUMBER' | 'PARAGRAPH' | 'PROMPT' | 'SAY' | 'STRING' | 'TEXT' | 'TO' | 'UNNAMED' | 'WEEKS'
whitespace = #'\\s+'
<text-word> = !keyword #'[\\w]+'
text = '\"' text-word { whitespace text-word } '\"' | text-word { whitespace text-word }
descriptor = text-word { whitespace text-word }
block = context | defaults | example | fact-declaration | (* include | *) order | rule | verbs
context = 'CONTEXT' descriptor
defaults = 'DEFAULT' generic-type 'STYLE' text
fact-declaration = [ 'GOAL' ] fact-type descriptor [ 'FROM' context ] [ 'PROVIDES' ] { attachment | explanation | info | prompt | range | translation } [ statements ]
fact-type = [qualifier] [type 'FACT' | 'FACT']
qualifier = 'SYSTEM' | 'UNREPORTED' | 'UNNAMED' | 'INFORMAL' | 'GENDER-NEUTRAL'
attachment = 'ATTACH' ['DISPLAYED'] [qualifier] 'REPORT'|'DOCUMENT'|'TEMPLATE' descriptor ['AS' descriptor]
explanation = 'EXPLAIN' [ 'UNKNOWN' | descriptor ] 'AS' text
info = 'INFO' text
prompt = 'PROMPT' text
range = 'RANGE' arith-expr [ 'TO' arith-expr ]
translation = 'TRANSLATE' [ 'RANGE' | 'UNKNOWN' | descriptor ] 'AS' text
fact-type = generic-type | named-subject
generic-type = 'BOOLEAN'|'DATE'|'GENDER'|'INTEGER'|'MONEY'|'NUMBER'|'STRING'
named-subject = 'PERSON' | 'THING' | 'PERSON-THING'
verbs = 'VERB' | 'VERBS' descriptor
(* include = 'INCLUDE' file-name *)
example = example-header example-body
example-header = ['GOAL'] 'EXAMPLE' ['RULE'] [descriptor] 'PROVIDES'
example-body = 'IF' bool-expr 'THEN' assignment
order = 'ORDER' descriptor { 'THEN' descriptor }
rule = rule-header statements
rule-header = ['GOAL'] rule-type ['RULE'] [descriptor] <'PROVIDES'>
rule-type = 'BACKWARD'|'DAEMON'|'DOCUMENT'|'FORWARD'|'PROCEDURE'|'RULE'
statements = statement+
statement = assignment|call|case|determine|exit|forget|if|(* include| *)repeat|say|while|write|'BEGIN' statements 'END'
<assignment> = [ 'ASSERT' ] (is-assignment | !is-assignment descriptor { 'AND' descriptor })
is-assignment = descriptor is-assignment-operator expression
is-assignment-operator = 'IS' | 'ONLY IF'
call = 'CALL' | 'SUBRULE' | 'NEXT' ['GOAL'] descriptor [ 'FROM' descriptor ]
case = 'CASE' descriptor { 'WHEN' descriptor [ 'THEN' ] statement }
determine = 'DETERMINE' descriptor
exit = 'EXIT' ['SESSION']
forget = 'FORGET' ( 'ALL' | descriptor )
if = 'IF' expression 'THEN' statement [ 'ELSE' statement ]
say = 'SAY' text
repeat = 'REPEAT' statements 'UNTIL' expression
write = ['NUMBERED'] ['LEVEL' natural-number] 'PARAGRAPH'|'LINE'|'TEXT' text
natural-number = digit { digit }
digit = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
while = 'WHILE' expression 'DO' statement
<expression> = bool-expr | !bool-expr arith-expr | !arith-expr rel-expr
<bool-expr> = fact-expr | !fact-expr and-or-expr | !and-or-expr and-expr | !and-expr or-expr
fact-expr = descriptor
and-or-expr = bool-expr { <'AND/OR'|'AND/OR/WITH'> bool-expr }
and-expr = bool-expr { <'AND'/'AND-WITH'> bool-expr }
or-expr = bool-expr { <'OR'/'OR-WITH'> bool-expr }
rel-expr = arith-expr { ['IS'] rel-op ['THAN'|'TO'] arith-expr }
arith-expr = term { 'PLUS'|'MINUS' } term
term = factor { 'TIMES'|'DIVIDED' ['BY'] } factor
factor = {pre-unary-op} descriptor [post-unary-op]
rel-op = 'LESS'|'GREATER'|'LESSEQUAL'|'GREATEREQUAL'|'EQUAL'|'EQUALS'|'NOT' 'EQUAL'|'NOT' 'EQUALS'
pre-unary-op = 'NOT'|'MINUS'|'PLUS'|'DAY'|'MONTH'|'YEAR'|'HOUR'|'MINUTE'|'SECOND'|'UNKNOWN'
post-unary-op = 'DAY'|'WEEK'|'MONTH'|'YEAR'|'DAYS'|'WEEKS'|'MONTHS'|'YEARS'|'HOURS'|'MINUTES'|'SECONDS'"
  :auto-whitespace :standard)
