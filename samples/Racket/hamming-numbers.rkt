#lang racket
(require racket/stream)
(define first stream-first)
(define rest  stream-rest)

(define (merge s1 s2)
  (define x1 (first s1))
  (define x2 (first s2))
  (cond [(= x1 x2) (merge s1 (rest s2))]
        [(< x1 x2) (stream-cons x1 (merge (rest s1) s2))]
        [else      (stream-cons x2 (merge s1 (rest s2)))]))

(define (mult k) (λ(x) (* x k)))

(define hamming
  (stream-cons
   1 (merge (stream-map (mult 2) hamming)
            (merge (stream-map (mult 3) hamming)
                   (stream-map (mult 5) hamming)))))

(for/list ([i 20] [x hamming]) x)
(stream-ref hamming 1690)
(stream-ref hamming 999999)
