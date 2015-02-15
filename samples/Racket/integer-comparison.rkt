#lang racket
(define (compare-two-ints a b)
  (define compared
    (cond ((> a b) "is greated than")
          ((= a b) "equals")
          ((< a b) "is lesser than")))
  (format "~a ~a ~a" a compared b))

(compare-two-ints (read) (read))
