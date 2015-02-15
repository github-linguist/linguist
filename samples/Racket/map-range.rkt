#lang racket

(define (make-range-map a1 a2 b1 b2)
  ;; returns a mapping function, doing computing the differences in
  ;; advance so it's fast
  (let ([a (- a2 a1)] [b (- b2 b1)])
    (λ(s) (exact->inexact (+ b1 (/ (* (- s a1) b) a))))))

(define map (make-range-map 0 10 -1 0))
(for ([i (in-range 0 11)]) (printf "~a --> ~a\n" i (map i)))
