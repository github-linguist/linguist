#lang racket

(require racket/date)

(define HALF-HOUR-SECS (* 60 30))

;; given a date, return the seconds corresponding to the beginning
;; of that day (in local time)
(define (beginning-of-date d)
  (find-seconds 0 0 0 (date-day d) (date-month d) (date-year d)))

;; the seconds at the beginning of today:
(define today-secs
  (beginning-of-date
    (seconds->date (current-seconds))))

;; hours -> watch : given an hour, return the watch name
(define (hours->watch hours)
  (cond [(= 0 hours) "first"]
        [(< 0 hours 4.5) "middle"]
        [(< 4 hours 8.5) "morning"]
        [(< 8 hours 12.5) "forenoon"]
        [(< 12 hours 16.5) "afternoon"]
        [(< 16 hours 20.5) "dog"]
        [(< 20 hours 24.5) "first"]))

;; wait until current-seconds is the given number
(define (wait-til secs)
  (sleep (- secs (current-seconds))))

;; display the appropriate message
(define (format-and-print hours bells)
  (define int-hours (floor hours))
  (define minutes (cond [(integer? hours) "00"]
                        [else "30"]))
  (display
   (~a
    (~a (floor hours) #:min-width 2 #:pad-string "0"
        #:align 'right)
    ":" minutes ", " bells " bell(s) of the "
    (hours->watch hours) " watch "))
  ;; play the bells, if possible:
  (for ([i bells])
    (display "\a♪")
    (flush-output)
    (cond [(even? i) (sleep 0.5)]
          [(odd? i) (display " ") (sleep 1)]))
  (display "\n"))

;; start the loop:
(for ([s (in-range today-secs +inf.0 HALF-HOUR-SECS)]
      [bells (sequence-tail (in-cycle (in-range 8)) 7)]
      [hours (in-cycle (in-range 0 24 1/2))])
  ;; ignore the ones that have already happened:
  (when (< (current-seconds) s)
    (wait-til s)
    (format-and-print hours (add1 bells))))
