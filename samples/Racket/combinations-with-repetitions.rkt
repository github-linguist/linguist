#lang racket
(define (combinations xs k)
  (cond [(= k 0)     '(())]
        [(empty? xs) '()]
        [(append (combinations (rest xs) k)
                 (map (λ(x) (cons (first xs) x))
                      (combinations xs (- k 1))))]))
