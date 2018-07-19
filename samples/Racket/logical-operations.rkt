#lang racket

(define (logic a b)
  (displayln (format "a and b equals ~a" (and a b)))
  (displayln (format "a or b equals ~a" (or a b)))
  (displayln (format "not a equals ~a" (not a)))
  (displayln (format "a nand b equals ~a" (nand a b)))
  (displayln (format "a nor b equals ~a" (nor a b)))
  (displayln (format "a implies b equals ~a" (implies a b)))
  (displayln (format "a xor b equals ~a" (xor a b))))
