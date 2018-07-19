#lang racket
(require (only-in srfi/43 vector-swap!))

(define (heap-sort! xs)
  (define (ref i) (vector-ref xs i))
  (define (swap! i j) (vector-swap! xs i j))
  (define size (vector-length xs))

  (define (sift-down! r end)
    (define c (+ (* 2 r) 1))
    (define c+1 (+ c 1))
    (when (<= c end)
      (define child
        (if (and (<= c+1 end) (< (ref c) (ref c+1)))
            c+1 c))
      (when (< (ref r) (ref child))
        (swap! r child))
      (sift-down! child end)))

  (for ([i (in-range (quotient (- size 2) 2) -1 -1)])
    (sift-down! i (- size 1)))

  (for ([end (in-range (- size 1) 0 -1)])
    (swap! 0 end)
    (sift-down! 0 (- end 1)))
  xs)
