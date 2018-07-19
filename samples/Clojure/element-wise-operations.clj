(defn initial-mtx [i1 i2 value]
  (vec (repeat i1 (vec (repeat i2 value)))))

(defn operation [f mtx1 mtx2]
  (if (vector? mtx1)
    (vec (map #(vec (map f %1 %2)) mtx1 mtx2)))
    (recur f (initial-mtx (count mtx2) (count (first mtx2)) mtx1) mtx2)
  ))
