#lang racket

(define (multi-factorial-fn m)
  (lambda (n)
    (let inner ((acc 1) (n n))
      (if (<= n m) (* acc n)
          (inner (* acc n) (- n m))))))

;; using (multi-factorial-fn m) as a first-class function
(for*/list ([m (in-range 1 (add1 5))] [mf-m (in-value (multi-factorial-fn m))])
  (for/list ([n (in-range 1 (add1 10))])
  (mf-m n)))

(define (multi-factorial m n) ((multi-factorial-fn m) n))

(for/list ([m (in-range 1 (add1 5))])
  (for/list ([n (in-range 1 (add1 10))])
  (multi-factorial m n)))
