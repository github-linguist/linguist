#lang racket

#| Euclid's enumeration formula and counting is fast enough for extra credit.

 For maximum perimeter P₀, the primitive triples are enumerated by n,m with:

   1 ≤ n < m
   perimeter P(n, m) ≤ P₀ where P(n, m) = (m² - n²) + 2mn + (m² + n²) = 2m(m+n)
   m and n of different parity and coprime.

 Since n < m, a simple close non-tight bound on n is P(n, n) < P₀.
 For each of these the exact set of m's can be enumerated.

 Each primitive triple with perimeter p represents one triple for each kp ≤ P₀,
  of which there are floor(P₀/p) k's. |#

(define (P n m) (* 2 m (+ m n)))
(define (number-of-triples P₀)
  (for/fold ([primitive 0] [all 0])
    ([n (in-naturals 1)]
     #:break (>= (P n n) P₀))
    (for*/fold ([primitive′ primitive] [all′ all])
      ([m (in-naturals (+ n 1))]
       #:break (> (P n m) P₀)
       #:when (and (odd? (- m n)) (coprime? m n)))
      (values (+ primitive′ 1)
              (+ all′ (quotient P₀ (P n m)))))))


(define (print-results P₀)
  (define-values (primitive all) (number-of-triples P₀))
  (printf "~a ~a:\n  ~a, ~a.\n"
          "Number of Pythagorean triples and primitive triples with perimeter ≤"
          P₀
          all primitive))
(print-results 100)
(time (print-results (* 100 1000 1000)))

#|
   Number of Pythagorean triples and primitive triples with perimeter ≤ 100:
     17, 7.
   Number of Pythagorean triples and primitive triples with perimeter ≤ 100000000:
     113236940, 7023027.
   cpu time: 11976 real time: 12215 gc time: 2381
|#
