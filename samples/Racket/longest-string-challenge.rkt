#lang racket

(define (newline? c) (equal? c #\newline))
(define eof? eof-object?)

(let loop ([O  '()] [C  '(#\newline)] [rI '()] [rO '()] [rC '()])
  (let* ([i (read-char)] [o (car C)] [i:rI (cons i rI)] [i:rC (cons i rC)])
    (cond [(eof? i) (for-each write-char O)]
          [(and (newline? o) (newline? i))
           (let ([O (reverse i:rC)]) (loop O O '() i:rC i:rC))]
          [(newline? i) (loop O O       '()  rO rO)]
          [(newline? o) (loop O C       i:rI rO i:rI)]
          [else         (loop O (cdr C) i:rI rO i:rC)])))
