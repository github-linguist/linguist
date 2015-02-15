#lang racket

(printf "Enter XY dimensions: ")
(define xy (cons (read) (read)))
(define array (for/vector ([x (car xy)]) (for/vector ([y (cdr xy)]) 0)))

(printf "Enter a number for the top-left: ")
(vector-set! (vector-ref array 0) 0 (read))
(printf "Enter a number for the bottom-right: ")
(vector-set! (vector-ref array (sub1 (car xy))) (sub1 (cdr xy)) (read))

array
