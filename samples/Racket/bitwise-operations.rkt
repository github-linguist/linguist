#lang racket
(define a 255)
(define b 5)
(list (bitwise-and a b)
      (bitwise-ior a b)
      (bitwise-xor a b)
      (bitwise-not a)
      (arithmetic-shift a b)      ; left shift
      (arithmetic-shift a (- b))) ; right shift
