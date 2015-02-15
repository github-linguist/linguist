(defrecord Vector [x y z])

(defn dot
  [U V]
  (+ (* (:x U) (:x V))
     (* (:y U) (:y V))
     (* (:z U) (:z V))))

(defn cross
  [U V]
  (new Vector
       (- (* (:y U) (:z V)) (* (:z U) (:y V)))
       (- (* (:z U) (:x V)) (* (:x U) (:z V)))
       (- (* (:x U) (:y V)) (* (:y U) (:x V)))))

(let [a (new Vector 3 4 5)
      b (new Vector 4 3 5)
      c (new Vector -5 -12 -13)]
  (doseq
    [prod (list
            (dot a b)
            (cross a b)
            (dot a (cross b c))
            (cross a (cross b c)))]
    (println prod)))
