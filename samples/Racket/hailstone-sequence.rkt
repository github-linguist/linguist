#lang racket

(define hailstone
  (let ([t (make-hasheq)])
    (hash-set! t 1 '(1))
    (λ(n) (hash-ref! t n
            (λ() (cons n (hailstone (if (even? n) (/ n 2) (+ (* 3 n) 1)))))))))

(define h27 (hailstone 27))
(printf "h(27) = ~s, ~s items\n"
        `(,@(take h27 4) ... ,@(take-right h27 4))
        (length h27))

(define N 100000)
(define longest
  (for/fold ([m #f]) ([i (in-range 1 (add1 N))])
    (define h (hailstone i))
    (if (and m (> (cdr m) (length h))) m (cons i (length h)))))
(printf "for x<=~s, ~s has the longest sequence with ~s items\n"
        N (car longest) (cdr longest))
