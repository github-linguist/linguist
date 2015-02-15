#lang racket

(define (nth-root number root (tolerance 0.001))
  (define (acceptable? next current)
    (< (abs (- next current)) tolerance))

  (define (improve current)
    (/ (+ (* (- root 1) current) (/ number (expt current (- root 1)))) root))

  (define (loop current)
    (define next-guess (improve current))
    (if (acceptable? next-guess current)
        next-guess
        (loop next-guess)))
  (loop 1.0))
