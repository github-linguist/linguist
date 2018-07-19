(defn biased [n]
  (if (< (rand 2) (/ n)) 0 1))

(defn unbiased [n]
  (loop [a 0 b 0]
    (if (= a b)
      (recur (biased n) (biased n))
      a)))

(for [n (range 3 7)]
  [n
   (double (/ (apply + (take 50000 (repeatedly #(biased n)))) 50000))
   (double (/ (apply + (take 50000 (repeatedly #(unbiased n)))) 50000))])
([3 0.83292 0.50422]
 [4 0.87684 0.5023]
 [5 0.90122 0.49728]
 [6 0.91526 0.5])
