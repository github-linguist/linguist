(defn ackermann [m n]
  (cond (zero? m) (inc n)
        (zero? n) (ackermann (dec m) 1)
        :else (ackermann (dec m) (ackermann m (dec n)))))
