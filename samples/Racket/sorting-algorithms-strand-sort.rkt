#lang racket
(require mzlib/list)
(define (merge xs ys) (merge-sorted-lists xs ys <=))

(define (strand-sort xs)
  (let loop ([xs xs] [ys '[]])
    (cond [(empty? xs) ys]
          [else (define-values (sorted unsorted) (extract-strand xs))
                (loop unsorted (merge sorted ys))])))

(define (extract-strand xs)
  (for/fold ([strand '()] [unsorted '[]]) ([x xs])
    (if (or (empty? strand) (< x (first strand)))
        (values (cons x strand) unsorted)
        (values strand (cons x unsorted)))))

(strand-sort (build-list 10 (λ(_) (random 15))))
