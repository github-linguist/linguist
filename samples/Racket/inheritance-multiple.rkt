#lang racket

(define camera<%>       (interface ()))
(define mobile-phone<%> (interface ()))

(define camera-phone%
  (class* object% (camera<%> mobile-phone<%>)
    (super-new)
    ;; implement methods here
    ))
