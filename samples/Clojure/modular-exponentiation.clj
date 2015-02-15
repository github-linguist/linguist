(defn powerMod "modular exponentiation" [b e m]
  (defn m* [p q] (mod (* p q) m))
  (loop [b b, e e, x 1]
    (if (zero? e) x
      (if (even? e) (recur (m* b b) (/ e 2) x)
        (recur (m* b b) (quot e 2) (m* b x))))))
