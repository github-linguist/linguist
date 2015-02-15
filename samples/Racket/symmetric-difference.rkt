#lang racket
(define A (set "John" "Bob" "Mary" "Serena"))
(define B (set "Jim" "Mary" "John" "Bob"))

(set-symmetric-difference A B)
(set-subtract A B)
(set-subtract B A)
