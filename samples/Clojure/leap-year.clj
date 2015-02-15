(defn leap-year? [y]
  (and (zero? (mod y 4)) (or (pos? (mod y 100)) (zero? (mod y 400)))))
