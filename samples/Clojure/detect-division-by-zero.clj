(defn safe-/ [x y]
  (try (/ x y)
    (catch ArithmeticException _
      (println "Division by zero caught!")
      (cond (> x 0)   Double/POSITIVE_INFINITY
            (zero? x) Double/NaN
            :else     Double/NEGATIVE_INFINITY) )))
