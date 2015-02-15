#lang racket

(define data '([Joe 5531] [Adam 2341] [Bernie 122] [Walter 1234] [David 19]))

(sort data < #:key cadr)
;; --> '((David 19) (Bernie 122) (Walter 1234) (Adam 2341) (Joe 5531))

;; Demonstrating a "key" that is not just a direct element
(sort data string<? #:key (compose1 symbol->string car))
;; --> '((Adam 2341) (Bernie 122) (David 19) (Joe 5531) (Walter 1234))
