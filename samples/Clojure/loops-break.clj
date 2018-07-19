(loop [[a b & more] (repeatedly #(rand-int 20))]
  (println a)
  (when-not (= 10 a)
    (println b)
    (recur more)))
