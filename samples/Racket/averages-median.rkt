#lang racket
(define (median numbers)
  (define sorted (list->vector (sort (vector->list numbers) <)))
  (define count (vector-length numbers))
  (if (zero? count)
      #f
      (/ (+ (vector-ref sorted (floor (/ (sub1 count) 2)))
            (vector-ref sorted (floor (/ count 2))))
         2)))

(median '#(5 3 4)) ;; 4
(median '#()) ;; #f
(median '#(5 4 2 3)) ;; 7/2
(median '#(3 4 1 -8.4 7.2 4 1 1.2)) ;; 2.1
