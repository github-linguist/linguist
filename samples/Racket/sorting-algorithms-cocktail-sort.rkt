#lang racket
(require (only-in srfi/43 vector-swap!))

(define (cocktail-sort! xs)
  (define (ref i) (vector-ref xs i))
  (define (swap i j) (vector-swap! xs i j))
  (define len (vector-length xs))
  (define (bubble from to delta)
    (for/fold ([swaps 0]) ([i (in-range from to delta)])
      (cond [(> (ref i) (ref (+ i 1)))
             (swap i (+ i 1)) (+ swaps 1)]
            [swaps])))
  (let loop ()
    (cond [(zero? (bubble 0 (- len 2)  1)) xs]
          [(zero? (bubble (- len 2) 0  -1)) xs]
          [(loop)])))
