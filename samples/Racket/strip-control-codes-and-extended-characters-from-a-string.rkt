#lang racket
;; Works on both strings (Unicode) and byte strings (raw/ASCII)
(define (strip-controls str)
  (regexp-replace* #rx"[\0-\037\177]+" str ""))
(define (strip-controls-and-extended str)
  (regexp-replace* #rx"[^\040-\176]+" str ""))
