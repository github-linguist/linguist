#lang racket

(define (fs f s) (map f s))
(define (f1 n) (* n 2))
(define (f2 n) (* n n))

(define fsf1 (curry fs f1))
(define fsf2 (curry fs f2))

(fsf1 '(0 1 2 3))
(fsf1 '(2 4 6 8))
(fsf2 '(0 1 2 3))
(fsf2 '(2 4 6 8))
