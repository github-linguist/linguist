#lang racket

;; import dynamic arrays
(require data/gvector)

(define v (vector 1 2 3 4))   ; array
(vector-ref v 0)              ; 1
(vector-set! v 1 4)           ; 2 -> 4

(define gv (gvector 1 2 3 4)) ; dynamic array
(gvector-ref gv 0)            ; 1
(gvector-add! gv 5)           ; increase size
