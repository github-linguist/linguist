(defn horner [coeffs x]
  (reduce (fn [acc coeff] (+ (* acc x) coeff)) (reverse coeffs)))

(println (horner [-19 7 -4 6] 3))
