#lang racket
(when #t (displayln "true"))
((displayln "true") . when . #t)

(define a 6)
(set! a 5)
(a . set! . 6)
