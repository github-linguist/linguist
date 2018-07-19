#lang racket
(define (F n)
  (if (>= 0 n)
      1
      (- n (M (F (sub1 n))))))

(define (M n)
  (if (>= 0 n)
      0
      (- n (F (M (sub1 n))))))
