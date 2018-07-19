#lang racket
(define (decode/roman number)
  (define letter-values
    (map cons '(#\M #\D #\C #\L #\X #\V #\I) '(1000 500 100 50 10 5 1)))
  (define (get-value letter)
    (cdr (assq letter letter-values)))
  (define lst (map get-value (string->list number)))
  (+ (last lst)
     (for/fold ((sum 0))
       ((i (in-list lst)) (i+1 (in-list (cdr lst))))
       (+ sum
          (if (> i+1 i)
              (- i)
              i)))))

(map decode/roman '("MCMXC" "MMVIII" "MDCLXVI"))
;-> '(1990 2008 1666)
