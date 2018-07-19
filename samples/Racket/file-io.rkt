#lang racket
(define file-content
  (with-input-from-file "input.txt"
    (lambda ()
      (let loop ((lst null))
        (define new (read-char))
        (if (eof-object? new)
            (apply string lst)
            (loop (append lst (list new))))))))

(with-output-to-file "output.txt"
  (lambda ()
    (write file-content)))
