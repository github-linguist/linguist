(defn transpose [xs]
  (loop [ret [], remain xs]
    (if (empty? remain)
      ret
      (recur (conj ret (map first remain))
             (filter not-empty (map rest remain))))))

(defn bead-sort [xs]
  (->> xs
       (map #(repeat 1 %))
       transpose
       transpose
       (map #(reduce + %))))

(-> [5 2 4 1 3 3 9] bead-sort println)
