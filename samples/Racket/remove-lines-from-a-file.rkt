#lang racket
(define (remove-lines file from num)
  (define lines (file->lines file))
  (define-values [pfx rest] (split-at lines (sub1 from)))
  (display-lines-to-file (append pfx (drop rest num)) file #:exists 'replace))
