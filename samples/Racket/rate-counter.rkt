#lang racket

;; Racket has a useful `time*' macro that does just what's requested:
;; run some expression N times, and produce timing results
(require unstable/time)

;; Sample use:
(define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))
(time* 10 (fib 38))

;; But of course, can be used to measure external processes too:
(time* 10 (system "sleep 1"))
