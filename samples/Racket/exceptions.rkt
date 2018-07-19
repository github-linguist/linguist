#lang racket

;; define a new exception type
(struct exn:my-exception exn ())

;; handler that prints the message ("Hi!")
(define (handler exn)
  (displayln (exn-message exn)))

;; install exception handlers
(with-handlers ([exn:my-exception? handler])
  ;; raise the exception
  (raise (exn:my-exception "Hi!" (current-continuation-marks))))
