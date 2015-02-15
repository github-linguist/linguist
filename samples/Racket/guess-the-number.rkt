#lang racket
(define (guess-number (number (add1 (random 10))))
  (define guess (read))
  (if (= guess number)
      (display "Well guessed!\n")
      (guess-number number)))
