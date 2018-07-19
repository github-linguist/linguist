#lang racket

(define (pancake-sort l)
  (define (flip l n) (append (reverse (take l n)) (drop l n)))
  (for/fold ([l l]) ([i (in-range (length l) 1 -1)])
    (let* ([i2 (cdr (for/fold ([m #f]) ([x l] [j i])
                      (if (and m (<= x (car m))) m (cons x j))))]
           [l (if (zero? i2) l (flip l (add1 i2)))])
      (flip l i))))

(pancake-sort (shuffle (range 0 10)))
;; => '(0 1 2 3 4 5 6 7 8 9)
