(defn findRoots [f start stop step eps]
      (filter #(-> (f %) Math/abs (< eps)) (range start stop step)))
