#lang racket
(require (only-in math factorial))

(define (analytical n)
  (for/sum ([i (in-range 1 (add1 n))])
    (/ (factorial n) (expt n i) (factorial (- n i)))))

(define (test n times)
  (define (count-times seen times)
    (define x (random n))
    (if (memq x seen) times (count-times (cons x seen) (add1 times))))
  (/ (for/fold ([count 0]) ([i times]) (count-times '() count))
     times))

(define (test-table max-n times)
  (displayln " n avg    theory error\n------------------------")
  (for ([i (in-range 1 (add1 max-n))])
    (define average    (test i times))
    (define theory     (analytical i))
    (define difference (* (abs (sub1 (/ average theory))) 100))
    (displayln (~a (~a i #:width 2 #:align 'right)
                   " " (real->decimal-string average 4)
                   " " (real->decimal-string theory 4)
                   " " (real->decimal-string difference 4)
                   "%"))))

(test-table 20 10000)
