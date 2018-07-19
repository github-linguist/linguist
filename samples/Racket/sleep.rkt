#lang racket
(displayln "Enter a time (in seconds): ")
(define time (read))
(when (number? time)
  (displayln "Sleeping...")
  (sleep time)
  (displayln "Awake!"))
