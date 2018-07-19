#!/usr/bin/env racket
#lang racket

(define (*write file data) ; write data in human readable format (sexpr/line)
  (with-output-to-file file #:exists 'replace
    (lambda () (for ([x data]) (printf "~s\n" x)))))
(define *read file->list) ; read our "human readable format"

(command-line
 #:once-any
 [("-a") file title category date "Add an entry"
  (*write file `(,@(*read file) (,title ,category ,date)))]
 [("-d") file title "Delete an entry (all matching)"
  (*write file (filter-not (lambda (x) (equal? (car x) title)) (*read file)))]
 [("-p") file mode "Print entries, mode = latest, latest/cat, all, by-date"
  (define data (*read file))
  (define (show item)
    (match item [(list title cat date) (printf "[~a] ~a; ~a\n" cat title date)]))
  (case (string->symbol mode)
    [(all) (for-each show data)]
    [(by-date) (for-each show (sort data string<? #:key cadr))]
    [(latest) (show (last data))]
    [(latest/cat)
     (define (last/cat c) (for/last ([x data] #:when (equal? c (cadr x))) x))
     (for-each (compose1 show last/cat) (remove-duplicates (map cadr data)))]
    [else (error 'sdb "bad printout mode")])])
