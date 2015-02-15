(defn maprange [[a1 a2] [b1 b2] s]
	(+ b1 (/ (* (- s a1) (- b2 b1)) (- a2 a1))))

> (doseq [s (range 11)]
       (printf "%2s maps to %s\n" s (maprange [0 10] [-1 0] s)))

 0 maps to -1
 1 maps to -9/10
 2 maps to -4/5
 3 maps to -7/10
 4 maps to -3/5
 5 maps to -1/2
 6 maps to -2/5
 7 maps to -3/10
 8 maps to -1/5
 9 maps to -1/10
10 maps to 0
