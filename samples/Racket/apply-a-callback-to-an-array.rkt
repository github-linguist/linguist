#lang racket

;; using the `for/vector' comprehension form
(for/vector ([i #(1 2 3 4 5)]) (sqr i))

;; the usual functional `map'
(vector-map sqr #(1 2 3 4 5))
