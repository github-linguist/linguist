#lang racket
(define (binary-search x v)
  ; loop : index index -> index or #f
  ;   return i s.t. l<=i<h and v[i]=x
  (define (loop l h)
    (cond [(>= l h) #f]
          [else (define m (quotient (+ l h) 2))
                (define y (vector-ref v m))
                (cond
                  [(> y x) (loop l (- m 1))]
                  [(< y x) (loop (+ m 1) h)]
                  [else m])]))
  (loop 0 (vector-length v)))
