#lang racket
(require math)
(define A (matrix
           [[1   3   5]
            [2   4   7]
            [1   1   0]]))

(matrix-lu A)
; result:
; (mutable-array #[#[1 0 0]
;                  #[2 1 0]
;                  #[1 1 1]])
; (mutable-array #[#[1 3 5]
;                  #[0 -2 -3]
;                  #[0 0 -2]])
