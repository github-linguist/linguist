#lang racket

;; accepts a list to sort
(define (sleep-sort lst)
  (define done (make-channel))
  (for ([elem lst])
    (thread
     (λ ()
       (sleep elem)
       (channel-put done elem))))
  (for/list ([_ (length lst)])
    (channel-get done)))

;; outputs '(2 5 5 7 8 9 10)
(sleep-sort '(5 8 2 7 9 10 5))
