#lang racket
(define ((accumulator n) i)
  (set! n (+ n i))
  n)
