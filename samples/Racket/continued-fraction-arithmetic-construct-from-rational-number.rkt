#lang racket

(define ((r2cf n d))
  (or (zero? d)
      (let-values ([(q r) (quotient/remainder n d)])
        (set! n d)
        (set! d r)
        q)))

(define (r->cf n d)
  (for/list ([i (in-producer (r2cf n d) #t)]) i))

(define (real->cf x places)
  (define d (expt 10 places))
  (define n (exact-floor (* x d)))
  (r->cf n d))

(map r->cf
     '(1 3 23 13 22 -151)
     '(2 1  8 11  7   77))
(real->cf (sqrt 2) 10)
(real->cf pi 10)
