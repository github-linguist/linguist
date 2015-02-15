#lang typed/racket

(define-type (Tree A) (U False (Node A)))

(struct: (A) Node
  ([val : A] [left : (Tree A)] [right : (Tree A)])
  #:transparent)

(: tree-map (All (A B) (A -> B) (Tree A) -> (Tree B)))
(define (tree-map f tree)
  (match tree
    [#f #f]
    [(Node val left right)
     (Node (f val) (tree-map f left) (tree-map f right))]))

;; unit tests
(require typed/rackunit)
(check-equal?
 (tree-map add1 (Node 5 (Node 3 #f #f) #f))
 (Node 6 (Node 4 #f #f) #f))
