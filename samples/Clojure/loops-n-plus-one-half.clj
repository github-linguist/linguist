; Functional version
(apply str (interpose ", " (range 1 11)))

; Imperative version
(loop [n 1]
   (printf "%d" n)
   (if (< n 10)
       (do
	(print ", ")
	(recur (inc n)))))
