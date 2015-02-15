#lang racket
(define (sort l)
  (for/first ([p (in-permutations l)] #:when (apply <= p)) p))
(sort '(6 1 5 2 4 3)) ; => '(1 2 3 4 5 6)
