(declare F) ; forward reference

(defn M [n]
  (if (zero? n)
    0
    (- n (F (M (dec n))))))

(defn F [n]
  (if (zero? n)
    1
    (- n (M (F (dec n))))))
