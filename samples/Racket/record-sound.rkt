#lang racket
(define (record n) (with-input-from-file "/dev/dsp" ( () (read-bytes n))))
(define (play bs)  (display-to-file bs "/dev/dsp" #:exists 'append))
(play (record 65536))
