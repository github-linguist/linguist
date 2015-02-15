(defn ro2ar [r]
  (->> (reverse r)
       (replace (zipmap "MDCLXVI" [1000 500 100 50 10 5 1]))
       (partition-by identity)
       (map (partial apply +))
       (reduce #(if (< %1 %2) (+ %1 %2) (- %1 %2)))))
