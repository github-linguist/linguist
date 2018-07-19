#lang racket
(define (sum-of-digits n base (sum 0))
  (if (= n 0)
      sum
      (sum-of-digits (quotient n base)
                     base
                     (+ (remainder n base) sum))))

(for-each
 (lambda (number-base-pair)
   (define number (car number-base-pair))
   (define base (cadr number-base-pair))
   (displayln (format "(~a)_~a = ~a" number base (sum-of-digits number base))))
 '((1 10) (1234 10) (#xfe 16) (#xf0e 16)))



;  outputs:
;    (1)_10 = 1
;    (1234)_10 = 10
;    (254)_16 = 29
;    (3854)_16 = 29
