(defn !! [m n]
  (apply * (take-while pos? (iterate #(- % m) n))))

(doseq [m (range 1 6)]
  (prn m (map #(!! m %) (range 1 11))))
