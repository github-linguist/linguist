#lang racket
(require rackunit)

(define (columns lst)
  (match (filter (λ (l) (not (empty? l))) lst)
    ['() '()]
    [l (cons (map car l) (columns (map cdr l)))]))

(define (bead-sort lst)
  (map length (columns (columns (map (λ (n) (make-list n 1)) lst)))))

;; unit test
(check-equal?
 (bead-sort '(5 3 1 7 4 1 1))
 '(7 5 4 3 1 1 1))
