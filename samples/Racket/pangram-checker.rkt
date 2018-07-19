#lang racket
(define (pangram? str)
  (define chars (regexp-replace* #rx"[^a-z]+" (string-downcase str) ""))
  (= 26 (length (remove-duplicates (string->list chars)))))
(pangram? "The quick Brown Fox jumps over the Lazy Dog")
