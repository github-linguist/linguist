(def precedence '{* 0, / 0
		  + 1, - 1})

(defn order-ops
  "((A x B) y C) or (A x (B y C)) depending on precedence of x and y"
  [[A x B y C & more]]
  (let [ret (if (<=  (precedence x)
		     (precedence y))
	      (list (list A x B) y C)
	      (list A x (list B y C)))]
    (if more
      (recur (concat ret more))
      ret)))

(defn add-parens
  "Tree walk to add parens.  All lists are length 3 afterwards."
  [s]
  (clojure.walk/postwalk
   #(if (seq? %)
      (let [c (count %)]
	(cond (even? c) (throw (Exception. "Must be an odd number of forms"))
	      (= c 1) (first %)
	      (= c 3) %
	      (>= c 5) (order-ops %)))
      %)
   s))

(defn make-ast
  "Parse a string into a list of numbers, ops, and lists"
  [s]
  (-> (format "'(%s)" s)
      (.replaceAll , "([*+-/])" " $1 ")
      load-string
      add-parens))

(def ops {'* *
	  '+ +
	  '- -
	  '/ /})

(def eval-ast
     (partial clojure.walk/postwalk
	      #(if (seq? %)
		 (let [[a o b] %]
		   ((ops o) a b))
		 %)))

(defn evaluate [s]
  "Parse and evaluate an infix arithmetic expression"
  (eval-ast (make-ast s)))

user> (evaluate "1 + 2*(3 - 2*(3 - 2)*((2 - 4)*5 - 22/(7 + 2*(3 - 1)) - 1)) + 1")
60
