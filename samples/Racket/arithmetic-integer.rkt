#lang racket
(define (arithmetic x y)
  (for ([op '(+ - * / quotient remainder modulo max min gcd lcm)])
    (displayln (~a (list op x y) " => "
                   ((eval op (make-base-namespace)) x y)))))

(arithmetic 8 12)
