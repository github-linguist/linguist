#lang racket
(define (^ base expt)
  (for/fold ((acum 1))
    ((i (in-range expt)))
    (* acum base)))

(^ 5 2) ; 25
(^ 5.0 2) ; 25.0
