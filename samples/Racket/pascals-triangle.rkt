#lang racket

(define (pascal n)
  (define (next-row current-row)
    (map + (cons 0 current-row)
           (append current-row '(0))))
  (let-values
      ([(previous-rows final-row)
       (for/fold ([triangle null]
                  [row '(1)])
         ([row-number (in-range 1 n)])
         (values (cons row triangle)
                 (next-row row)))])
    (reverse (cons final-row previous-rows))))
