#lang racket

(define lang-names '("X" "Y" "Z"))

(define rx
  (regexp (string-join lang-names "|"
                       #:before-first "<((/?(?:code)?)(?:( )?("
                       #:after-last "))?)>")))

(let loop () ; does all in a single scan
  (define m (regexp-match rx (current-input-port) 0 #f (current-output-port)))
  (when m
    (define-values [all pfx space lang] (apply values (cdr m)))
    (printf "<~a>"
      (cond [(not lang) (if (equal? pfx #"/code") #"/lang" all)]
            [space (if (equal? pfx #"code") (bytes-append #"lang " lang) all)]
            [(equal? pfx #"") (bytes-append #"lang " lang)]
            [(equal? pfx #"/") #"/lang"]
            [else all]))
    (loop)))
