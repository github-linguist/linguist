#lang racket
(require math)
(define (reduced-echelon M)
  (matrix-row-echelon M #t #t))

(reduced-echelon
 (matrix [[1 2 -1 -4]
          [2 3 -1 -11]
          [-2 0 -3 22]]))
