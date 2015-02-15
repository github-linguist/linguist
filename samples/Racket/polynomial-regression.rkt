#lang racket
(require math plot)

(define xs '(0 1  2  3  4  5   6   7   8   9  10))
(define ys '(1 6 17 34 57 86 121 162 209 262 321))

(define (fit x y n)
  (define Y (->col-matrix y))
  (define V (vandermonde-matrix x (+ n 1)))
  (define VT (matrix-transpose V))
  (matrix->vector (matrix-solve (matrix* VT V) (matrix* VT Y))))

(define ((poly v) x)
  (for/sum ([c v] [i (in-naturals)])
    (* c (expt x i))))

(plot (list (points   (map vector xs ys))
            (function (poly (fit xs ys 2)))))
