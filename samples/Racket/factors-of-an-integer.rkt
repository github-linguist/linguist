#lang racket

;; a naive version
(define (naive-factors n)
  (for/list ([i (in-range 1 (add1 n))] #:when (zero? (modulo n i))) i))
(naive-factors 120) ; -> '(1 2 3 4 5 6 8 10 12 15 20 24 30 40 60 120)

;; much better: use `factorize' to get prime factors and construct the
;; list of results from that
(require math)
(define (factors n)
  (sort (for/fold ([l '(1)]) ([p (factorize n)])
          (append (for*/list ([e (in-range 1 (add1 (cadr p)))] [x l])
                    (* x (expt (car p) e)))
                  l))
        <))
(naive-factors 120) ; -> same

;; to see how fast it is:
(define huge 1200034005600070000008900000000000000000)
(time (length (factors  huge)))
;; I get 42ms for getting a list of 7776 numbers

;; but actually the math library comes with a `divisors' function that
;; does the same, except even faster
(divisors 120) ; -> same

(time (length (divisors huge)))
;; And this one clocks at 17ms
