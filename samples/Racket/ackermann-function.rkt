#lang racket
(define (ackermann m n)
  (cond [(zero? m) (add1 n)]
        [(zero? n) (ackermann (sub1 m) 1)]
        [else (ackermann (sub1 m) (ackermann m (sub1 n)))]))
