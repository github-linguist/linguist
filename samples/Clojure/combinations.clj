(defn combinations
  "If m=1, generate a nested list of numbers [0,n)
   If m>1, for each x in [0,n), and for each list in the recursion on [x+1,n), cons the two"
  [m n]
  (letfn [(comb-aux
	   [m start]
	   (if (= 1 m)
	     (for [x (range start n)]
	       (list x))
	     (for [x (range start n)
		   xs (comb-aux (dec m) (inc x))]
	       (cons x xs))))]
    (comb-aux m 0)))

(defn print-combinations
  [m n]
  (doseq [line (combinations m n)]
    (doseq [n line]
      (printf "%s " n))
    (printf "%n")))
