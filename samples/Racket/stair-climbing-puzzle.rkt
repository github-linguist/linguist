#lang racket
(define p 0.5001)
(define (step)
  (> p (random)))

(define (step-up n)
  (cond ((zero? n) 'done)
        ((step) (step-up (sub1 n)))
        (else (step-up (add1 n)))))

(step-up 1)
