#lang typed/racket

(: S : Natural -> Real)
(define (S n)
  (for/sum: : Real ([k : Natural (in-range 1 (+ n 1))])
    (/ 1.0 (* k k))))
