(loop [i 0]
  (println i)
  (when (< i 10)
    (recur (+ 2 i))))

(doseq [i (range 0 12 2)]
  (println i))
