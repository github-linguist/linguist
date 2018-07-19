#lang racket
(require math)
(define running-stddev
  (let ([ns '()])
    (λ(n) (set! ns (cons n ns)) (stddev ns))))
;; run it on each number, return the last result
(last (map running-stddev '(2 4 4 4 5 5 7 9)))
