#lang racket
(printf "Input a string: ")
(define s (read-line))
(printf "You entered: ~a\n" s)

(printf "Input a number: ")
(define m (or (string->number (read-line))
              (error "I said a number!")))
(printf "You entered: ~a\n" m)

;; alternatively, use the generic `read'
(printf "Input a number: ")
(define n (read))
(unless (number? n) (error "I said a number!"))
(printf "You entered: ~a\n" n)
