#lang racket

(define (all-misplaced? l)
  (for/and ([x (in-list l)] [n (in-naturals 1)]) (not (= x n))))

(define (topswops n)
  (for/fold ([m 0]) ([p (in-permutations (range 1 (add1 n)))]
                     #:when (all-misplaced? p))
    (let loop ([p p] [n 0])
      (if (= 1 (car p))
        (max n m)
        (loop (let loop ([l '()] [r p] [n (car p)])
                (if (zero? n) (append l r)
                    (loop (cons (car r) l) (cdr r) (sub1 n))))
              (add1 n))))))

(for ([i (in-range 1 11)]) (printf "~a\t~a\n" i (topswops i)))
