#lang racket
(define (print text)
  ;; try lpr first
  (define lpr-exe (find-executable-path "lpr"))
  ;; otherwise use a special file
  (if lpr-exe
    (with-input-from-string (~a text "\n") (λ() (void (system* lpr-exe))))
    (with-output-to-file #:exists 'append
      (case (system-type) [(windows) "PRN"] [else "/dev/lp0"])
      (λ() (displayln text)))))
(print "Hello World!")
