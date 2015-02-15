#lang racket

(define (luhn-test n)
  (let loop ([n n] [odd? #t] [s 0])
    (if (zero? n)
      (zero? (modulo s 10))
      (let*-values ([(q r)   (quotient/remainder n 10)]
                    [(rq rr) (quotient/remainder (* (if odd? 1 2) r) 10)])
        (loop q (not odd?) (+ s rq rr))))))

(map luhn-test '(49927398716 49927398717 1234567812345678 1234567812345670))
;; -> '(#t #f #f #t)
