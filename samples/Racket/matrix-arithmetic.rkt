#lang racket
(require math)
(define determinant matrix-determinant)

(define (permanent M)
  (define n (matrix-num-rows M))
  (for/sum ([σ (in-permutations (range n))])
    (for/product ([i n] [σi σ])
      (matrix-ref M i σi))))
