#lang racket
(define (perfect? n)
  (= n
     (for/fold ((sum 0))
       ((i (in-range 1 (add1 (floor (/ n 2))))))
       (if (= (remainder n i) 0)
           (+ sum i)
           sum))))


(filter perfect? (build-list 1000 values))
;-> '(0 6 28 496)
