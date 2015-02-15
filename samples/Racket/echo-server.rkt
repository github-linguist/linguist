#lang racket

(define listener (tcp-listen 12321))
(let echo-server ()
  (define-values [I O] (tcp-accept listener))
  (thread (λ() (copy-port I O) (close-output-port O)))
  (echo-server))
