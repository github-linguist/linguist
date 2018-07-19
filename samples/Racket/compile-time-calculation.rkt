#lang racket

;; Import the math library for compile-time
;; Note: included in Racket v5.3.2
(require (for-syntax math))

;; In versions older than v5.3.2, just define the function
;; for compile-time
;;
;; (begin-for-syntax
;;   (define (factorial n)
;;     (if (zero? n)
;;         1
;;         (factorial (- n 1)))))

;; define a macro that calls factorial at compile-time
(define-syntax (fact10 stx)
  #`#,(factorial 10))

;; use the macro defined above
(fact10)
