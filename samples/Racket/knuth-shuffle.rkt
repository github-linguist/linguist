#lang racket

(define (swap! vec i j)
  (let ([tmp (vector-ref vec i)])
    (vector-set! vec i (vector-ref vec j))
    (vector-set! vec j tmp)))

(define (knuth-shuffle x)
  (if (list? x)
    (vector->list (knuth-shuffle (list->vector x)))
    (begin (for ([i (in-range (sub1 (vector-length x)) 0 -1)])
             (define r (random i))
             (swap! x i r))
           x)))

(knuth-shuffle '(1 2 3 4))
