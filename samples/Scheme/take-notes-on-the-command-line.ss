#lang racket
(require racket/date)
(define *notes* "NOTES.TXT")

(let ([a (vector->list (current-command-line-arguments))])
  (cond
    [(empty? a)
     (with-handlers ([exn:fail? void])
       (call-with-input-file *notes*
         (lambda (fi)
           (copy-port fi (current-output-port)))))
     ]
    [else
     (call-with-output-file *notes*
       (lambda (fo)
         (let ([ln (apply string-append (add-between a " "))]
               [dt (date->string (current-date))])
           (fprintf fo "~a~n\t~a~n" dt ln)))
       #:mode 'text #:exists 'append)
     ]))
