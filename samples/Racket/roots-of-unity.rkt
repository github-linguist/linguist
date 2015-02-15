#lang racket

(define (roots-of-unity n)
  (for/list ([k n])
    (make-polar 1 (* k (/ (* 2 pi) n)))))
