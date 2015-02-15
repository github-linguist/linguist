#lang racket

(define answer (number->string (foldr expt 1 '(5 4 3 2))))
(define len (string-length answer))

(printf "Got ~a digits~n" len)
(printf "~a ... ~a~n"
        (substring answer 0 20)
        (substring answer (- len 20) len))
