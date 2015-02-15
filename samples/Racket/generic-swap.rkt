#lang racket/load

(module swap racket
  (provide swap)

  ;; a simple macro to swap two variables
  (define-syntax-rule (swap a b)
    (let ([tmp a])
      (set! a b)
      (set! b tmp))))

;; works fine in a statically typed setting
(module typed typed/racket
  (require 'swap)

  (: x Integer)
  (define x 3)

  (: y Integer)
  (define y 4)

  (swap x y)
  (printf "x is ~a~n" x)
  (printf "y is ~a~n" y))
