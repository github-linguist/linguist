#lang racket

(define (encode str)
  (regexp-replace* #px"(.)\\1*" str (lambda (m c) (~a (string-length m) c))))

(define (look-and-say-sequence n)
  (reverse (for/fold ([r '("1")]) ([n n]) (cons (encode (car r)) r))))

(for-each displayln (look-and-say-sequence 10))
