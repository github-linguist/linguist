#lang racket

;; Like other Lisps, Racketeers prefer using symbols directly instead of
;; numeric definitions, and lists of symbols instead of bitwise
;; combinations
(define fruits '(apple banana cherry))

;; In Typed Racket, a type can be defined for a specific set of symbols
;; (define-type Fruit (U 'apple 'banana 'cherry))

;; The conventional approach is possible too, of course
(define APPLE  1)
(define BANANA 2)
(define CHERRY 4)

;; And finally, when dealing with foreign functions it is useful to
;; translate idiomatic Racket values (= symbols) to/from integers.
;; Racket's ffi has two ways to do this -- either an enumeration (for
;; independent integer constants) or a bitmask (intended to represent
;; sets using bitwise or):
(require ffi/unsafe)
(define _fruit  (_enum '(APPLE = 1
                         BANANA
                         CHERRY = 4)))
(define _fruits (_bitmask '(APPLE = 1
                            BANANA = 2
                            CHERRY = 4)))

;; Normally, Racket code will just use plain values (a symbol for the
;; first, and a list of symbols for the second) and the foreign side
;; sees the integers.  But do demonstrate this, we can use the primitive
;; raw functionality to see how the translation works:
(require (only-in '#%foreign ctype-scheme->c ctype-c->scheme))

((ctype-scheme->c _fruit)  'CHERRY)         ; -> 4
((ctype-scheme->c _fruits) 'CHERRY)         ; -> 4
((ctype-scheme->c _fruits) '(APPLE CHERRY)) ; -> 5

((ctype-c->scheme _fruit)  4) ; -> 'CHERRY
((ctype-c->scheme _fruits) 4) ; -> '(CHERRY)
((ctype-c->scheme _fruits) 5) ; -> '(APPLE CHERRY)
