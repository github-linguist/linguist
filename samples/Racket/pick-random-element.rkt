#lang racket
(define (pick-item l)
  (list-ref l (random (length l))))
