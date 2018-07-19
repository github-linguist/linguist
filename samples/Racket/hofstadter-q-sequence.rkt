#lang racket

(define t (make-hash))
(hash-set! t 0 0)
(hash-set! t 1 1)
(hash-set! t 2 1)

(define (Q n)
  (hash-ref! t n (λ() (+ (Q (- n (Q (- n 1))))
                         (Q (- n (Q (- n 2))))))))

(for/list ([i (in-range 1 11)]) (Q i))
(Q 1000)

;; extra credit
(for/sum ([i 100000]) (if (< (Q (add1 i)) (Q i)) 1 0))
