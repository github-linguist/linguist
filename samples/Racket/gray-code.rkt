#lang racket

(define (gray-encode n) (bitwise-xor n (arithmetic-shift n -1)))

(define (gray-decode n)
  (letrec ([loop (lambda(g bits)
                   (if (> bits 0)
                       (loop (bitwise-xor g bits) (arithmetic-shift bits -1))
                       g))])
	 (loop 0 n)))

(define (to-bin n) (format "~b" n))
(define (show-table)
  (for ([i (in-range 1 32)])
    (printf "~a | ~a | ~a ~n"
            (~r i #:min-width 2 #:pad-string "0")
            (~a (to-bin(gray-encode i)) #:width 5 #:align 'right #:pad-string "0")
            (~a (to-bin (gray-decode(gray-encode i))) #:width 5 #:align 'right #:pad-string "0"))))
