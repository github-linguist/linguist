#lang racket

;; a-lists
(define a-list '((a . 5) (b . 10)))
(assoc a-list 'a) ; => '(a . 5)

;; hash tables
(define table #hash((a . 5) (b . 10)))
(hash-ref table 'a) ; => 5

;; dictionary interface
(dict-ref a-list 'a) ; => 5
(dict-ref table 'a)  ; => 5
