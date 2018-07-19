#lang racket

(define (truncate file size)
  (unless (file-exists? file) (error 'truncat "missing file: ~a" file))
  (when (> size (file-size file)) (printf "Warning: extending file size.\n"))
  (call-with-output-file* file #:exists 'update
    (λ(o) (file-truncate o size))))
