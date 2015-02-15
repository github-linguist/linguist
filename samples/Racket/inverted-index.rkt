#!/usr/bin/env racket
#lang racket
(command-line
 #:args (term . files)
 (define rindex (make-hasheq))
 (for ([file files])
   (call-with-input-file file
     (λ(in) (let loop ()
              (define w (regexp-match #px"\\w+" in))
              (when w
                (let* ([w (bytes->string/utf-8 (car w))]
                       [w (string->symbol (string-foldcase w))]
                       [r (hash-ref rindex w '())])
                  (unless (member file r) (hash-set! rindex w (cons file r)))
                  (loop)))))))
 (define res
   (for/list ([w (regexp-match* #px"\\w+" term)])
     (list->set (hash-ref rindex (string->symbol (string-foldcase w)) '()))))
 (define all (set->list (apply set-intersect res)))
 (if (null? all)
   (printf "No matching files.\n")
   (printf "Terms found at: ~a.\n" (string-join all ", "))))
