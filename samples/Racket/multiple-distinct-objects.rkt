#lang racket

;; a list of 10 references to the same vector
(make-list 10 (make-vector 10 0))

;; a list of 10 distinct vectors
(build-list 10 (λ (n) (make-vector 10 0)))
