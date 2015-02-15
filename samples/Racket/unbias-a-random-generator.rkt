#lang racket
;; Using boolean #t/#f instead of 1/0
(define ((randN n)) (zero? (random n)))
(define ((unbiased biased))
  (let loop () (let ([r (biased)]) (if (eq? r (biased)) (loop) r))))

;; Counts
(define N 1000000)
(for ([n (in-range 3 7)])
  (define (try% R) (round (/ (for/sum ([i N]) (if (R) 1 0)) N 1/100)))
  (define biased (randN n))
  (printf "Count: ~a => Biased: ~a%; Unbiased: ~a%.\n"
          n (try% biased) (try% (unbiased biased))))
