#lang racket

(define x  2.0)
(define xi 0.5)
(define y  4.0)
(define yi 0.25)
(define z  (+ x y))
(define zi (/ 1.0 (+ x y)))

(define ((multiplier x y) z) (* x y z))

(define numbers  (list x  y  z))
(define inverses (list xi yi zi))

(for/list ([n numbers] [i inverses])
  ((multiplier n i) 0.5))
;; -> '(0.5 0.5 0.5)
