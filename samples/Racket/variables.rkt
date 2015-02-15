#lang racket

;; define a variable and initialize it
(define foo 0)
;; increment it
(set! foo (add1 foo))

;; Racket is lexically scoped, which makes local variables work:
(define (bar)
  (define foo 100)
  (set! foo (add1 foo))
  foo)
(bar) ; -> 101

;; and it also makes it possible to have variables with a global value
;; that are accessible only in a specific local lexical scope:
(define baz
  (let () ; used to create a new scope
    (define foo 0)
    (define (bar)
      (set! foo (add1 foo))
      foo)
    bar)) ; this is the value that gets bound to `baz'
(list (baz) (baz) (baz)) ; -> '(1 2 3)

;; define a new type, and initialize a variable with an instance
(struct pos (x y))
(define p (pos 1 2))
(list (pos-x p) (pos-y p)) ; -> '(1 2)

;; for a mutable reference, a struct (or some specific fields in a
;; struct) can be declared mutable
(struct mpos (x y) #:mutable)
(define mp (mpos 1 2))
(set-mpos-x! mp 11)
(set-mpos-y! mp 22)
(list (mpos-x mp) (mpos-y mp)) ; -> '(11 22)

;; but for just a mutable value, we have boxes as a builtin type
(define b (box 10))
(set-box! b (add1 (unbox b)))
(unbox b) ; -> 11

;; (Racket has many more related features: static typing in typed
;; racket, structs that are extensions of other structs,
;; pattern-matching on structs, classes, and much more)
