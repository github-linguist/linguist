#lang racket

(define (mode seq)
  (define frequencies (make-hash))
  (for ([s seq])
    (hash-update! frequencies
                  s
                  (lambda (freq) (add1 freq))
                  0))
  (for/fold ([ms null]
             [freq 0])
            ([(k v) (in-hash frequencies)])
    (cond [(> v freq)
           (values (list k) v)]
          [(= v freq)
           (values (cons k ms) freq)]
          [else
           (values ms freq)])))
