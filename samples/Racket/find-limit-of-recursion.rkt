#lang racket
(define (recursion-limit)
  (with-handlers ((exn? (lambda (x) 0)))
    (add1 (recursion-limit))))
