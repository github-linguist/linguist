#lang racket

;; Calling a function that requires no arguments
(foo)

;; Calling a function with a fixed number of arguments
(foo 1 2 3)

;; Calling a function with optional arguments
;; Calling a function with a variable number of arguments
(foo 1 2 3) ; same in both cases

;; Calling a function with named arguments
(foo 1 2 #:x 3) ; using #:keywords for the names

;; Using a function in statement context
;; Using a function in first-class context within an expression
;; Obtaining the return value of a function
;; -> Makes no sense for Racket, as well as most other functional PLs

;; Distinguishing built-in functions and user-defined functions
(primitive? foo)
;; but this is mostly useless, since most of Racket is implemented in
;; itself

;; Distinguishing subroutines and functions
;; -> No difference, though `!' is an idiomatic suffix for names of
;;    side-effect functions, and they usually return (void)

;; Stating whether arguments are passed by value or by reference

;; -> Always by value, but it's possible to implement languages with
;;    other argument passing styles, including passing arguments by
;;    reference (eg, there is "#lang algol60")

;; Is partial application possible and how
(curry foo 1 2)    ; later apply this on 3
(λ(x) (foo 1 2 x)) ; a direct way of doing the same
