#lang racket

(define (halve  i) (quotient i 2))
(define (double i) (* i 2))
;; `even?' is built-in

(define (ethiopian-multiply x y)
  (cond [(zero? x) 0]
        [(even? x) (ethiopian-multiply (halve x) (double y))]
        [else (+ y (ethiopian-multiply (halve x) (double y)))]))

(ethiopian-multiply 17 34) ; -> 578
