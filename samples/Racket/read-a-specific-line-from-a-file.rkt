#lang racket

;; simple, but reads the whole file
(define s1 (list-ref (file->lines "some-file") 6))

;; more efficient: read and discare n-1 lines
(define s2
  (call-with-input-file "some-file"
    (λ(i) (for/last ([line (in-lines i)] [n 7]) line))))
