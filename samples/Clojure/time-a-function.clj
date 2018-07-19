  (defn fib []
    (map first
      (iterate
        (fn [[a b]] [b (+ a b)])
        [0 1])))

  (time (take 100 (fib)))
