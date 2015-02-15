#lang racket
(require math/number-theory)

(define (truncate-right n)
  (quotient n 10))

(define (truncate-left n)
  (define s (number->string n))
  (string->number (substring s 1 (string-length s))))

(define (contains-zero? n)
  (member #\0 (string->list (number->string n))))

(define (truncatable? truncate n)
  (and (prime? n)
       (not (contains-zero? n))
       (or (< n 10)
           (truncatable? truncate (truncate n)))))

; largest left truncatable prime
(for/first ([n (in-range 1000000 1 -1)]
            #:when (truncatable? truncate-left n))
  n)

; largest right truncatable prime
(for/first ([n (in-range 1000000 1 -1)]
            #:when (truncatable? truncate-right n))
  n)

; Output:
998443
739399
