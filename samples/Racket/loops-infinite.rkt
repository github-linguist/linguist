#lang racket

;; Using recursion
(define (loop)
  (displayln "SPAM")
  (loop))

(loop)

;; Using a for loop
(for ([i (in-naturals)])
  (displayln "SPAM"))
