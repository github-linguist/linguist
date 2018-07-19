#lang racket

(define input
  (for/list ([i 11])
    (printf "Enter a number (~a of 11): " (+ 1 i))
    (read)))

(for ([x (reverse input)])
  (define res (+ (sqrt (abs x)) (* 5 (expt x 3))))
  (if (> res 400)
      (displayln "Overflow!")
      (printf "f(~a) = ~a\n" x res)))
