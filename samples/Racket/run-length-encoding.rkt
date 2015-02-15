#lang racket
(define (encode str)
  (regexp-replace* #px"(.)\\1*" str (λ (m c) (~a (string-length m) c))))
(define (decode str)
  (regexp-replace* #px"([0-9]+)(.)" str (λ (m n c) (make-string (string->number n) (string-ref c 0)))))
