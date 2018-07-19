(defn dot-product [& matrix]
  {:pre [(apply == (map count matrix))]}
  (apply + (apply map * matrix)))

;Example Usage
(println (dot-product [1 3 -5] [4 -2 -1]))
