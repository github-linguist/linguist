#!/bin/env racket
#lang racket
(define file "NOTES.TXT")
(require racket/date)
(command-line #:args notes
  (if (null? notes)
    (if (file-exists? file)
      (call-with-input-file* file
        (λ(i) (copy-port i (current-output-port))))
      (raise-user-error 'notes "missing ~a file" file))
    (call-with-output-file* file #:exists 'append
      (λ(o) (fprintf o "~a\n\t~a\n"
                     (date->string (current-date) #t)
                     (string-join notes))))))
