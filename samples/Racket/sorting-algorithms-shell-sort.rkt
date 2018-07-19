#lang racket
(define (shell-sort! xs)
  (define (ref i) (vector-ref xs i))
  (define (new Δ) (if (= Δ 2) 1 (exact-floor (/ (* Δ 5) 11))))
  (let loop ([Δ (quotient (vector-length xs) 2)])
    (unless (= Δ 0)
      (for ([xi (in-vector xs)] [i (in-naturals)])
        (let while ([i i])
          (cond [(and (>= i Δ) (> (ref (- i Δ)) xi))
                 (vector-set! xs i (ref (- i Δ)))
                 (while (- i Δ))]
                [(vector-set! xs i xi)])))
      (loop (new Δ))))
  xs)
