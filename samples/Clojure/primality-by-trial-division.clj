(defn divides? [k n] (= (rem n k) 0))

(defn prime? [n]
  (if (< n 2)
    false
    (empty? (filter #(divides? % n) (take-while #(<= (* % %) n) (range 2 n))))))
