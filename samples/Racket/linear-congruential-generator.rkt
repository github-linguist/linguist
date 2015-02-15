#lang racket
(require racket/generator)

(define (bsd-update state_n)
  (modulo (+ (* 1103515245 state_n) 12345)
          (expt 2 31)))

(define (ms-update state_n)
  (modulo (+ (* 214013 state_n) 2531011)
          (expt 2 31)))

(define ((rand update ->rand) seed)
  (generator ()
   (let loop ([state_n seed])
     (define state_n+1 (update state_n))
     (yield (->rand state_n+1))
     (loop state_n+1))))

(define bsd-rand (rand bsd-update identity))
(define ms-rand (rand ms-update (λ (x) (quotient x (expt 2 16)))))
