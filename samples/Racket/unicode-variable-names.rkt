#lang racket

;; Racket can use Unicode in identifier names
(define √ sqrt)
(√ 256) ; -> 16
;; and in fact the standard language makes use of some of these
(λ(x) x) ; -> an identity function

;; The required binding:
(define Δ 1)
(set! Δ (add1 Δ))
(printf "Δ = ~s\n" Δ) ; prints "Δ = 2"
