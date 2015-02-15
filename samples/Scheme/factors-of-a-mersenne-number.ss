#lang scheme

;;; this needs to be changed for other R6RS implementations
(require rnrs/arithmetic/bitwise-6)

;;; modpow, as per the task description.
(define (modpow exponent base)
  (let loop ([square 1] [index (- (bitwise-length exponent) 1)])
    (if (< index 0)
        square
        (loop (modulo (* (if (bitwise-bit-set? exponent index) 2 1)
                      square square) base)
              (- index 1)))))

;;; search through all integers from 1 on to find the first divisor
;;; returns #f if 2^p-1 is prime
(define (mersenne-factor p)
  (for/first ((i (in-range 1 (floor (expt 2 (quotient p 2))) (* 2 p)))
              #:when (and (or (= 1 (modulo i 8)) (= 7 (modulo i 8)))
                          (= 1 (modpow p i))))
    i))
