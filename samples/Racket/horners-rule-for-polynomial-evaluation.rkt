#lang racket
(define (horner x l)
    (foldr (lambda (a b) (+ a (* b x))) 0 l))

(horner 3 '(-19 7 -4 6))
