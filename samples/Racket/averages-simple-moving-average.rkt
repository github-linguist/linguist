#lang racket

(require data/queue)

(define (simple-moving-average period)
  (define queue (make-queue))
  (define sum 0.0)

  (lambda (x)
    (enqueue! queue x)
    (set! sum (+ sum x))
    (when (> (queue-length queue) period)
      (set! sum (- sum (dequeue! queue))))
    (/ sum (queue-length queue))))

;; Tests
(define sma3 (simple-moving-average 3))
(define sma5 (simple-moving-average 5))
(for/lists (lst1 lst2)
           ([i '(1 2 3 4 5 5 4 3 2 1)])
  (values (sma3 i) (sma5 i)))
