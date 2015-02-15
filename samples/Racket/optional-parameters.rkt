#lang racket

(define (sort-table table
                    [ordering string<=?]
                    [column 0]
                    [reverse? #f])
  (sort table (if reverse?
                  (negate ordering)
                  ordering)
        #:key (λ (row) (list-ref row column))))
