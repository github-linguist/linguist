#lang racket

(define s "I am a string")

(when (regexp-match? #rx"string$" s)
  (displayln "Ends with 'string'."))

(unless (regexp-match? #rx"^You" s)
  (displayln "Does not start with 'You'."))

(displayln (regexp-replace " a " s " another "))
