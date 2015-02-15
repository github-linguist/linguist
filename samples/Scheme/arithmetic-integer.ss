(define (arithmetic x y)
  (for-each (lambda (op)
              (write  (list op x y))
              (display " => ")
              (write ((eval op) x y))
              (newline))
            '(+ - * / quotient remainder modulo max min gcd lcm)))

(arithmetic 8 12)
