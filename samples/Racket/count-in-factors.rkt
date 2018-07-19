#lang racket
(require math)

(define (~ f)
  (match f
    [(list p 1) (~a p)]
    [(list p n) (~a p "^" n)]))

(for ([x (in-range 2 20)])
  (display (~a x " = "))
  (for-each display (add-between (map ~ (factorize x)) " * "))
  (newline))
