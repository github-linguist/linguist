;string ports

(define *output-strings* '())

(define open-output-string
  (let ((n 0))
    (lambda ()
      (let ((f (string-append *jobname* "-Z-SC-"
                 (number->string n) ".scm")))
        (set! n (+ n 1))
        (if (file-exists? f)
            ;try again
            (open-output-string)
            (let ((o (open-output-file f)))
              (set! *output-strings*
                (cons (cons o f) *output-strings*))
              o))))))

(define get-output-string
  (lambda (o)
    (let ((o-f (assv o *output-strings*)))
      (let ((o (car o-f))
            (f (cdr o-f)))
        (close-output-port o)
        (let ((s (call-with-input-file f
                   (lambda (o)
                     (let loop ((r '()))
                       (let ((c (read-char o)))
                         (if (eof-object? c)
                             (list->string (reverse! r))
                             (loop (cons c r)))))))))
          (delete-file f)
          s)))))



