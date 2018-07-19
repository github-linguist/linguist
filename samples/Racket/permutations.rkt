#lang racket

;; using a builtin
(permutations '(A B C))
;; -> '((A B C) (B A C) (A C B) (C A B) (B C A) (C B A))

;; a random simple version (which is actually pretty good for a simple version)
(define (perms l)
  (let loop ([l l] [tail '()])
    (if (null? l) (list tail)
        (append-map (λ(x) (loop (remq x l) (cons x tail))) l))))
(perms '(A B C))
;; -> '((C B A) (B C A) (C A B) (A C B) (B A C) (A B C))
