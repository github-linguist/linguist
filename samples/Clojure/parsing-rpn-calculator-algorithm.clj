(ns rosettacode.parsing-rpn-calculator-algorithm
  (:require clojure.math.numeric-tower
            clojure.string
            clojure.pprint))

(def operators
  "the only allowable operators for our calculator"
  {"+" +
   "-" -
   "*" *
   "/" /
   "^" clojure.math.numeric-tower/expt})

(defn rpn
  "takes a string and returns a lazy-seq of all the stacks"
  [string]
  (letfn [(rpn-reducer [stack item] ; this takes a stack and one item and makes a new stack
            (if (contains? operators item)
              (let [operand-1 (peek stack) ; if we used lists instead of vectors, we could use destructuring, but stacks would look backwards
                    stack-1 (pop stack)]   ;we're assuming that all the operators are binary
                (conj (pop stack-1)
                      ((operators item) (peek stack-1) operand-1)))
              (conj stack (Long. item))))] ; if it wasn't an operator, we'll assume it's a long. Could choose bigint, or even read-line
    (reductions rpn-reducer [] (clojure.string/split string #"\s+")))) ;reductions is like reduce only shows all the intermediate steps

(let [stacks (rpn "3 4 2 * 1 5 - 2 3 ^ ^ / +")] ;bind it so we can output the answer separately.
  (println "stacks: ")
  (clojure.pprint/pprint stacks)
  (print "answer:" (->> stacks last first)))
