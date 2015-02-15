#lang racket
(require (only-in srfi/43 vector-swap!))

(define (comb-sort xs)
  (define (ref i) (vector-ref xs i))
  (define (swap i j) (vector-swap! xs i j))
  (define (new gap) (max 1 (exact-floor (/ gap 1.25))))
  (define size (vector-length xs))
  (let loop ([gap size] [swaps 0])
    (unless (and (= gap 1) (= swaps 0))
      (loop (new gap)
            (for/fold ([swaps 0]) ([i (in-range 0 (- size gap))])
              (cond
                [(> (ref i) (ref (+ i gap)))
                 (swap i (+ i gap))
                 (+ swaps 1)]
                [swaps])))))
  xs)
