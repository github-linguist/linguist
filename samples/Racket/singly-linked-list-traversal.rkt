#lang racket

(define l (list 1 2 3))

;; scan the list and collect a list of function results
(map add1 l)

;; scan the list and run some function on each element for its side-effect
(for-each displayln l)

;; scan a list and sum up its elements
(foldl + 0 l)

;; same as the above three, using a more modern syntax that is often
;; more convenient
(for/list ([x (in-list l)]) (add1 x))
(for ([x (in-list l)]) (displayln x))
(for/fold ([sum 0]) ([x (in-list l)]) (+ x sum))

;; the same as the first, but make up a vector of results
(for/vector ([x (in-list l)]) (add1 x))

;; there is less support for mutable pairs, but it's still extensive
;; enough to cover all the basics
(require racket/mpair)
(define ml (mlist 1 2 3))
(mmap add1 ml)
(mfor-each displayln ml)
(for ([x (in-mlist ml)]) (displayln x))
