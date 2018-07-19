#lang racket

(require racket/system)
(define (flash str)
  (system "tput smcup")
  (displayln str)
  (sleep 2)
  (system "tput rmcup")
  (void))

(flash "Hello world.")
