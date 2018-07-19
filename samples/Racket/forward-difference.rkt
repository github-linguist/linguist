#lang racket

(define (forward-difference list)
  (for/list ([x (cdr list)] [y list]) (- x y)))

(define (nth-forward-difference n list)
  (for/fold ([list list]) ([n n]) (forward-difference list)))


(nth-forward-difference 9 '(90 47 58 29 22 32 55 5 55 73))
;; -> '(-2921)
