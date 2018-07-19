#lang racket
(define (sum-of-squared-digits number (result 0))
  (if (zero? number)
      result
      (sum-of-squared-digits (quotient number 10)
                             (+ result (expt (remainder number 10) 2)))))

(define (happy-number? number (seen null))
  (define next (sum-of-squared-digits number))
  (cond ((= 1 next)
         #t)
        ((memq next seen)
         #f)
        (else
         (happy-number? next (cons number seen)))))

(define (get-happys max)
  (for/list ((x (in-range max))
             #:when (happy-number? x))
    x))

(display (take (get-happys 100) 8)) ;displays (1 7 10 13 19 23 28 31)
