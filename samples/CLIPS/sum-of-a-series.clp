(deffunction S (?x) (/ 1 (* ?x ?x)))
(deffunction partial-sum-S
  (?start ?stop)
  (bind ?sum 0)
  (loop-for-count (?i ?start ?stop) do
    (bind ?sum (+ ?sum (S ?i)))
  )
  (return ?sum)
)
