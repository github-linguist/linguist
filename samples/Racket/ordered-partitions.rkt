#lang racket
(define (comb k xs)
  (cond [(zero? k)  (list (cons '() xs))]
        [(null? xs) '()]
        [else (append (for/list ([cszs (comb (sub1 k) (cdr xs))])
                        (cons (cons (car xs) (car cszs)) (cdr cszs)))
                      (for/list ([cszs (comb k (cdr xs))])
                        (cons (car cszs) (cons (car xs) (cdr cszs)))))]))
(define (partitions xs)
  (define (p xs ks)
    (if (null? ks)
      '(())
      (for*/list ([cszs (comb (car ks) xs)] [rs (p (cdr cszs) (cdr ks))])
        (cons (car cszs) rs))))
  (p (range 1 (add1 (foldl + 0 xs))) xs))

(define (run . xs)
  (printf "partitions~s:\n" xs)
  (for ([x (partitions xs)]) (printf "  ~s\n" x))
  (newline))

(run 2 0 2)
(run 1 1 1)
