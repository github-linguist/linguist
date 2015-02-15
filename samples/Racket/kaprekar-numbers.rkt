#lang racket
(define (kaprekar? n)
  (or (= n 1)
      (let ([q (sqr n)])
        (let loop ((p 10))
          (and (<= p q)
               (or (let-values  ([(b a) (quotient/remainder q p)])
                     (and (> a 0) (= n (+ a b))))
                   (loop (* p 10))))))))

(filter kaprekar? (range 1 10000))
