#lang racket(require math/array)

(define mat (list->array #(2 2) '(1 3 2 4)))

mat
(array+ mat (array 2))
(array* mat (array 2))
(array-map expt mat (array 2))

(array+ mat mat)
(array* mat mat)
(array-map expt mat mat)
