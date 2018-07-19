#lang racket

(define A (char->integer #\A))
(define Z (char->integer #\Z))
(define a (char->integer #\a))
(define z (char->integer #\z))

(define (rotate c n)
  (define cnum (char->integer c))
  (define (shift base) (integer->char (+ base (modulo (+ n (- cnum base)) 26))))
  (cond [(<= A cnum Z) (shift A)]
        [(<= a cnum z) (shift a)]
        [else c]))

(define (caesar s n)
  (list->string (for/list ([c (in-string s)]) (rotate c n))))

(define (encrypt s) (caesar s 1))
(define (decrypt s) (caesar s -1))
