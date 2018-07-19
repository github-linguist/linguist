#lang racket

(define (list->ranges xs)
  (define (R lo hi)
    (if (= lo hi) (~a lo) (~a lo (if (= 1 (- hi lo)) "," "-") hi)))
  (let loop ([xs xs] [lo #f] [hi #f] [r '()])
    (cond [(null? xs) (string-join (reverse (if lo (cons (R lo hi) r) r)) ",")]
          [(not hi) (loop (cdr xs) (car xs) (car xs) r)]
          [(= 1 (- (car xs) hi)) (loop (cdr xs) lo (car xs) r)]
          [else (loop xs #f #f (cons (R lo hi) r))])))

(list->ranges '(0 1 2 4 6 7 8 11 12 14 15 16 17 18 19 20 21 22 23
                24 25 27 28 29 30 31 32 33 35 36 37 38 39))
;; -> "0-2,4,6-8,11,12,14-25,27-33,35-39"
