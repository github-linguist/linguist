#lang racket
(for/list ([i 1000])
  (add1 (* (sqrt (* -2 (log (random)))) (cos (* 2 pi (random))) 0.5)))
