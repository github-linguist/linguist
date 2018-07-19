#lang racket
(define/contract (additive-persistence/digital-root n (ap 0))
  (->* (natural-number/c) (natural-number/c) (values natural-number/c natural-number/c))
  (define/contract (sum-digits x (acc 0))
    (->* (natural-number/c) (natural-number/c) natural-number/c)
    (if (= x 0)
        acc
        (let-values (((q r) (quotient/remainder x 10)))
          (sum-digits q (+ acc r)))))
  (if (< n 10)
      (values ap n)
      (additive-persistence/digital-root (sum-digits n) (+ ap 1))))

(module+ test
  (require rackunit)

  (for ((n (in-list '(627615 39390 588225 393900588225)))
        (ap (in-list '(2 2 2 2)))
        (dr (in-list '(9 6 3 9))))
    (call-with-values
      (lambda () (additive-persistence/digital-root n))
      (lambda (a d)
        (check-equal? a ap)
        (check-equal? d dr)
        (printf ":~a has additive persistence ~a and digital root of ~a;~%" n a d)))))
