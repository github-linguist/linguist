#lang racket
(require math)

(define a (matrix ((3 2) (2 1))))

;; Using the builtin matrix exponentiation
(for ([i 11])
  (printf "a^~a = ~s\n" i (matrix-expt a i)))

;; Output:
;; a^0 = (array #[#[1 0] #[0 1]])
;; a^1 = (array #[#[3 2] #[2 1]])
;; a^2 = (array #[#[13 8] #[8 5]])
;; a^3 = (array #[#[55 34] #[34 21]])
;; a^4 = (array #[#[233 144] #[144 89]])
;; a^5 = (array #[#[987 610] #[610 377]])
;; a^6 = (array #[#[4181 2584] #[2584 1597]])
;; a^7 = (array #[#[17711 10946] #[10946 6765]])
;; a^8 = (array #[#[75025 46368] #[46368 28657]])
;; a^9 = (array #[#[317811 196418] #[196418 121393]])
;; a^10 = (array #[#[1346269 832040] #[832040 514229]])

;; But it could be implemented manually, using matrix multiplication
(define (mpower M p)
  (cond [(= p 1) M]
        [(even? p) (mpower (matrix* M M) (/ p 2))]
        [else (matrix* M (mpower M (sub1 p)))]))
(for ([i (in-range 1 11)])
  (printf "a^~a = ~s\n" i (matrix-expt a i)))
