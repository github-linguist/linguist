#lang racket

(define animal% (class object% (super-new)))
(define dog%    (class animal% (super-new)))
(define cat%    (class animal% (super-new)))
(define lab%    (class dog% (super-new)))
(define collie% (class dog% (super-new)))

;; unit tests
(require rackunit)

(check-true (is-a? (new dog%) animal%))
(check-false (is-a? (new collie%) cat%))
