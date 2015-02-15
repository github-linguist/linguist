(doseq
  [n (range 1 11)]
  (do
    (print n)
    (if (= (rem n 5) 0) (print "\n") (print ", "))))
