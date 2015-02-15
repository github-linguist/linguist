#lang racket
(define (josephus n k (m 0))
  (for/fold ((m (add1 m)))
    ((a (in-range (add1 m) (add1 n))))
    (remainder (+ m k) a)))

(josephus 41 3) ; ->30
