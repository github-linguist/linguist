#lang racket
(define (rms nums)
  (sqrt (/ (for/sum ([n nums]) (* n n)) (length nums))))
