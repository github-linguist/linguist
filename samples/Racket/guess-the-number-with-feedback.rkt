#lang racket
(define min 1)
(define max 10)

(define (guess-number (number (+ min (random max))))
  (define guess (read))
  (cond ((not (number? guess)) (display "That's not a number!\n" (guess-number number)))
        ((or (> guess max) (> min guess)) (display "Out of range!\n") (guess-number number))
        ((> guess number) (display "Too high!\n") (guess-number number))
        ((< guess number) (display "Too low!\n") (guess-number number))
        (else (display "Well guessed!\n"))))

(display (format "Guess a number between ~a and ~a\n" min max))
(guess-number)
