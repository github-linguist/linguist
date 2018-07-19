#lang racket
(define (largest-int ns)
  (string->number (apply ~a (sort ns (λ(x y) (string>? (~a x y) (~a y x)))))))
(map largest-int '((1 34 3 98 9 76 45 4) (54 546 548 60)))
;; -> '(998764543431 6054854654)
