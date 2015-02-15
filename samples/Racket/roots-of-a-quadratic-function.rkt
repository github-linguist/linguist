#lang racket
(define (quadratic a b c)
  (let* ((-b (- b))
         (delta (- (expt b 2) (* 4 a c)))
         (denominator (* 2 a)))
    (list
     (/ (+ -b (sqrt delta)) denominator)
     (/ (- -b (sqrt delta)) denominator))))

;(quadratic 1 0.0000000000001 -1)
;'(0.99999999999995 -1.00000000000005)
;(quadratic 1 0.0000000000001 1)
;'(-5e-014+1.0i -5e-014-1.0i)
