#lang racket
(define (a x)
  (display (~a "a:" x " "))
  x)

(define (b x)
  (display (~a "b:" x " "))
  x)

(for* ([x '(#t #f)]
       [y '(#t #f)])
  (displayln `(and (a ,x) (b ,y)))
  (and (a x) (b y))
  (newline)

  (displayln `(or (a ,x) (b ,y)))
  (or (a x) (b y))
  (newline))
