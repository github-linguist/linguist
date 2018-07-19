#lang racket

;; insert b after a in a mutable list (assumes that a is in the input list)
(define (insert-after! list a b)
  (if (equal? (mcar list) a)
    (set-mcdr! list (mcons b (mcdr list)))
    (insert-after! (mcdr list) a b)))

(define l (mcons 1 (mcons 2 (mcons 3 '()))))
(insert-after! l 2 2.5)
l ; -> (mcons 1 (mcons 2 (mcons 2.5 (mcons 3))))
