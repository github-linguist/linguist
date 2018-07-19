#lang racket

(define (sort-disjoint l is)
  (define xs
    (sort (for/list ([x l] [i (in-naturals)] #:when (memq i is)) x) <))
  (let loop ([l l] [i 0] [xs xs])
    (cond [(null? l) l]
          [(memq i is) (cons (car xs) (loop (cdr l) (add1 i) (cdr xs)))]
          [else        (cons (car l)  (loop (cdr l) (add1 i) xs))])))

(sort-disjoint '(7 6 5 4 3 2 1 0) '(6 1 7))
;; --> '(7 0 5 4 3 2 1 6)
