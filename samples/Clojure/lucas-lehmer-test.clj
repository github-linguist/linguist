(defn prime? [i]
  (cond (< i 4)           (>= i 2)
        (zero? (rem i 2)) false
  :else (not-any? #(zero? (rem i %)) (range 3 (inc (Math/sqrt i))))))))

(defn mersenne? [p] (or (= p 2)
  (let [mp   (dec (bit-shift-left 1 p))]
    (loop [n 3 s 4]
      (if (> n p)
        (zero? s)
        (recur (inc n) (rem (- (* s s) 2) mp)))))))

(filter mersenne? (filter prime? (iterate inc 1)))
