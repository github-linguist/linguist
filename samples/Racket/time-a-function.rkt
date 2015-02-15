#lang racket
(define (fact n) (if (zero? n) 1 (* n (fact (sub1 n)))))
(time (fact 5000))
