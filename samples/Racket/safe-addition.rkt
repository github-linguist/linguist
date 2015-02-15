#lang racket

;; 1. Racket has exact unlimited integers and fractions, which can be
;;    used to perform exact operations.  For example, given an inexact
;;    flonum, we can convert it to an exact fraction and work with that:
(define (exact+ x y)
  (+ (inexact->exact x) (inexact->exact y)))
;; (A variant of this would be to keep all numbers exact, so the default
;; operations never get to inexact numbers)

;; 2. We can implement the required operation using a bunch of
;;    functionality provided by the math library, for example, use
;;    `flnext' and `flprev' to get the surrounding numbers for both
;;    inputs and use them to produce the resulting interval:
(require math)
(define (interval+ x y)
  (cons (+ (flprev x) (flprev y)) (+ (flnext x) (flnext y))))
(interval+ 1.14 2000.0) ; -> '(2001.1399999999999 . 2001.1400000000003)
;; (Note: I'm not a numeric expert in any way, so there must be room for
;; improvement here...)

;; 3. Yet another option is to use the math library's bigfloats, with an
;;    arbitrary precision:
(bf-precision 1024) ; 1024 bit floats
;; add two numbers, specified as strings to avoid rounding of number
;; literals
(bf+ (bf "1.14") (bf "2000.0"))
