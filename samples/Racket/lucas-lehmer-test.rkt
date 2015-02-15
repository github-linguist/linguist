#lang racket
(require math)

(define (mersenne-prime? p)
  (divides? (- (expt 2 p) 1) (S (- p 1))))

(define (S n)
  (if (= n 1) 4 (- (sqr (S (- n 1))) 2)))

(define (loop p)
  (when (mersenne-prime? p)
    (displayln p))
  (loop (next-prime p)))

(loop 3)
