#lang racket
(define (selection-sort xs)
  (cond [(empty? xs) '()]
        [else (define x0 (apply min xs))
              (cons x0 (selection-sort (remove x0 xs)))]))
