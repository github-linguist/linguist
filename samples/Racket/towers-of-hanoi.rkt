#lang racket
(define (hanoi n a b c)
  (when (> n 0)
    (hanoi (- n 1) a c b)
    (printf "Move ~a to ~a\n" a b)
    (hanoi (- n 1) c b a)))
(hanoi 4 'left 'middle 'right)
