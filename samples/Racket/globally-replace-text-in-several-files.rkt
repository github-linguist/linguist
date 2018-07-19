#!/usr/bin/env racket
#lang racket

(define from-string #f)
(define to-string #f)

(command-line
 #:once-each
 [("-f") from "Text to remove" (set! from-string from)]
 [("-t") to "Text to put instead" (set! to-string to)]
 #:args files
 (unless from-string (error "No `from' string specified"))
 (unless to-string   (error "No `to' string specified"))
 (when (null? files) (error "No files given"))
 (define from-rx (regexp (regexp-quote from-string)))
 (for ([file files])
   (printf "Editing ~a..." file) (flush-output)
   (define text1 (file->string file))
   (define text2 (regexp-replace* from-rx text1 to-string))
   (if (equal? text1 text2)
     (printf " no change\n")
     (begin (display-to-file text2 file #:exists 'replace)
            (printf " modified copy saved in place\n")))))
