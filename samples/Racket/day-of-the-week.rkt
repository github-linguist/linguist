#lang racket

(require racket/date)

(define (xmas-on-sunday? year)
  (zero? (date-week-day (seconds->date (find-seconds 0 0 12 25 12 year)))))

(for ([y (in-range 2008 2121)] #:when (xmas-on-sunday? y))
  (displayln y))
