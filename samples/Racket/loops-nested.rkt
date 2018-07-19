#lang racket
(define (scan xss)
  (for* ([xs xss]
         [x  xs]
         #:final (= x 20))
    (displayln x)))

(define matrix
  (for/list ([x 10])
    (for/list ([y 10])
      (+ (random 20) 1))))

(scan matrix)
