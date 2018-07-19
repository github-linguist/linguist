#lang racket/gui
(define-values [W H]
  (let ([f (new frame% [label "test"])])
    (begin0 (send* f (maximize #t) (show #t) (get-client-size))
      (send f show #f))))
(printf "~ax~a\n" W H)
