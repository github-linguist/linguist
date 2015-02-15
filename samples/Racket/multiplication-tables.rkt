#lang racket

(define (show-line xs)
  (for ([x xs]) (display (~a x #:width 4 #:align 'right)))
  (newline))

(show-line (cons "" (range 1 13)))
(for ([y (in-range 1 13)])
  (show-line (cons y (for/list ([x (in-range 1 13)])
                       (if (<= y x) (* x y) "")))))
