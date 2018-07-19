#lang racket

(define (code ch)
  (printf "The unicode number for ~s is ~a\n" ch (char->integer ch)))
(code #\a)
(code #\λ)

(define (char n)
  (printf "The unicode number ~a is the character ~s\n" n (integer->char n)))
(char 97)
(char 955)
