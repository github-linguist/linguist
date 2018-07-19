#lang racket
(define (bogo-sort l) (if (apply <= l) l (bogo-sort (shuffle l))))

(require rackunit)
(check-equal? (bogo-sort '(6 5 4 3 2 1)) '(1 2 3 4 5 6))
(check-equal? (bogo-sort (shuffle '(1 1 1 2 2 2))) '(1 1 1 2 2 2))

(let ((unsorted (for/list ((i 10)) (random 1000))))
  (displayln unsorted)
  (displayln (bogo-sort unsorted)))
