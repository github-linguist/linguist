#lang racket

(define a 3+4i)
(define b 8+0i)

(+ a b)       ; addition
(- a b)       ; subtraction
(/ a b)       ; division
(* a b)       ; multiplication
(- a)         ; negation
(/ 1 a)       ; reciprocal
(conjugate a) ; conjugation
