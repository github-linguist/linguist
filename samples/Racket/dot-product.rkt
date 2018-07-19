#lang racket
(define (dot-product l r) (for/sum ([x l] [y r]) (* x y)))

(dot-product '(1 3 -5) '(4 -2 -1))

;; dot-product works on sequences such as vectors:
(dot-product #(1 2 3) #(4 5 6))
