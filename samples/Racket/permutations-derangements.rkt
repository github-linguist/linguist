#lang racket

(define (all-misplaced? l)
  (for/and ([x (in-list l)] [n (in-naturals 1)]) (not (= x n))))

;; 1. Create a named function to generate derangements of the integers 0..n-1.
(define (derangements n)
  (define (all-misplaced? l1 l2)
    (or (null? l1)
        (and (not (eq? (car l1) (car l2)))
             (all-misplaced? (cdr l1) (cdr l2)))))
  (define l (range n))
  (for/list ([p (permutations l)] #:when (all-misplaced? p l))
    p))

;; 2. Generate and show all the derangements of 4 integers using the above
;;    routine.
(derangements 4)
;; -> '((1 0 3 2) (3 0 1 2) (1 3 0 2) (2 0 3 1) (2 3 0 1)
;;      (3 2 0 1) (1 2 3 0) (2 3 1 0) (3 2 1 0))

;; 3. Create a function that calculates the subfactorial of n, !n.
(define (sub-fact n)
  (if (< n 2) (- 1 n)
      (* (+ (sub-fact (- n 1)) (sub-fact (- n 2))) (sub1 n))))

;; 4. Print and show a table of the counted number of derangements of n vs. the
;;    calculated !n for n from 0..9 inclusive.
(for ([i 10])
  (printf "~a ~a ~a\n" i
          (~a #:width 7 #:align 'right (length (derangements i)))
          (sub-fact i)))
;; Output:
;; 0       1 1
;; 1       0 0
;; 2       1 1
;; 3       2 2
;; 4       9 9
;; 5      44 44
;; 6     265 265
;; 7    1854 1854
;; 8   14833 14833
;; 9  133496 133496

;; Extra: !20
(sub-fact 20)
;; -> 895014631192902121
