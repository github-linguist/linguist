#lang racket

(define (string-reverse s)
  (list->string (reverse (string->list s))))

(string-reverse "aoeu")
