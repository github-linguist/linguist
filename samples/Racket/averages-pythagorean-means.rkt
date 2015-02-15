#lang racket

(define (arithmetic xs)
  (/ (for/sum ([x xs]) x)
     (length xs)))

(define (geometric xs)
  (expt (for/product ([x xs]) x)
        (/ (length xs))))

(define (harmonic xs)
  (/ (length xs)
     (for/sum ([x xs]) (/ x))))

(define xs (range 1 11))
(arithmetic xs)
(geometric xs)
(harmonic xs)
(>= (arithmetic xs) (geometric xs) (harmonic xs))
