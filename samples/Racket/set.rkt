#lang racket

(define A (set 1 2 3 4))
(define B (set 3 4 5 6))
(define C (set 4 5))

(set-union A B)     ; gives (set 1 2 3 4 5 6)
(set-intersect A B) ; gives (set 3 4)
(set-subtract A B)  ; gives (set 1 2)
(set=? A B)         ; gives #f
(subset? C A)       ; gives #f
(subset? C B)       ; gives #t
