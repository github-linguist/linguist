#lang racket

(define (add-at l i x)
  (if (zero? i) (cons x l) (cons (car l) (add-at (cdr l) (sub1 i) x))))

(define (permutations l)
  (define (loop l)
    (cond [(null? l) '(())]
          [else (for*/list ([(p i) (in-indexed (loop (cdr l)))]
                            [i ((if (odd? i) identity reverse)
                                (range (add1 (length p))))])
                  (add-at p i (car l)))]))
  (for/list ([p (loop (reverse l))] [i (in-cycle '(1 -1))]) (cons i p)))

(define (show-permutations l)
  (printf "Permutations of ~s:\n" l)
  (for ([p (permutations l)])
    (printf "  ~a (~a)\n" (apply ~a (add-between (cdr p) ", ")) (car p))))

(for ([n (in-range 3 5)]) (show-permutations (range n)))
