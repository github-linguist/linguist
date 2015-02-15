#lang racket

(define (prime? number)
  (cond ((not (positive? number)) #f)
        ((= 1 number) #f)
        ((even? number) (= 2 number))
        (else (for/and ((i (in-range 3 (ceiling (sqrt number)))))
                (not (zero? (remainder number i)))))))
