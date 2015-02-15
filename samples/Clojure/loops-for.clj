(doseq [i (range 5), j (range (inc i))]
  (print "*")
  (if (= i j) (println)))
