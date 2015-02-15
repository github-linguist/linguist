(letfn [(a [bool] (print "(a)") bool)
        (b [bool] (print "(b)") bool)]
  (doseq [i [true false] j [true false]]
    (print i "OR" j "= ")
    (println (or (a i) (b j)))
    (print i "AND" j " = ")
    (println (and (a i) (b j)))))
