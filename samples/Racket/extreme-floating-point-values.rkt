#lang racket
(define division-by-zero (/ 1.0 0.0))      ;+inf.0
(define negative-inf (- (/ 1.0 0.0)))      ;-inf.0
(define zero 0.0)                          ;0.0
(define negative-zero (- 0.0))             ;-0.0
(define nan (/ 0.0 0.0))                   ;+nan.0

(displayln division-by-zero)
(displayln negative-inf)
(displayln zero)
(displayln negative-zero)
(displayln nan)

(+ zero negative-zero) ;0.0
(- negative-inf division-by-zero) ; +nan.0
(+ zero nan) ; +nan.0
(= nan +nan.0) ;#f
