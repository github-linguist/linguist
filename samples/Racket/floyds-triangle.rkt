#lang racket
(require math)

(define (tri n)
  (if (zero? n) 0 (triangle-number n)))

(define (floyd n)
  (define (width x) (string-length (~a x)))
  (define (~n x c) (~a x
                       #:width (width (+ (tri (- n 1)) 1 c))
                       #:align 'right #:left-pad-string " "))
  (for ([r n])
    (for ([c (+ r 1)])
      (display (~a (~n (+ (tri r) 1 c) c) " ")))
    (newline)))

(floyd 5)
(floyd 14)
