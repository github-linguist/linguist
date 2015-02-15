#lang racket

(define (non-square n)
  (+ n (exact-floor (+ 1/2 (sqrt n)))))

(map non-square (range 1 23))

(define (square? n) (integer? (sqrt n)))

(for/or ([n (in-range 1 1000001)])
  (square? (non-square n)))
