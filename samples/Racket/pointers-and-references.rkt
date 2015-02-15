#lang racket

(define (inc! b) (set-box! b (add1 (unbox b))))

(define b (box 0))
(inc! b)
(inc! b)
(inc! b)
(unbox b) ; => 3
