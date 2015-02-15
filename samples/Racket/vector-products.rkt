#lang racket

(define (dot-product X Y)
  (for/sum ([x (in-vector X)] [y (in-vector Y)]) (* x y)))

(define (cross-product X Y)
  (define len (vector-length X))
  (for/vector ([n len])
    (define (ref V i) (vector-ref V (modulo (+ n i) len)))
    (- (* (ref X 1) (ref Y 2)) (* (ref X 2) (ref Y 1)))))

(define (scalar-triple-product X Y Z)
  (dot-product X (cross-product Y Z)))

(define (vector-triple-product X Y Z)
  (cross-product X (cross-product Y Z)))

(define A '#(3 4 5))
(define B '#(4 3 5))
(define C '#(-5 -12 -13))

(printf "A = ~s\n" A)
(printf "B = ~s\n" B)
(printf "C = ~s\n" C)
(newline)

(printf "A . B = ~s\n" (dot-product A B))
(printf "A x B = ~s\n" (cross-product A B))
(printf "A . B x C = ~s\n" (scalar-triple-product A B C))
(printf "A x B x C = ~s\n" (vector-triple-product A B C))
