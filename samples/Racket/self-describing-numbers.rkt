#lang racket
(define (get-digits number (lst null))
  (if (zero? number)
      lst
      (get-digits (quotient number 10) (cons (remainder number 10) lst))))

(define (self-describing? number)
  (if (= number 0) #f
      (let ((digits (get-digits number)))
        (for/fold ((bool #t))
          ((i (in-range (length digits))))
          (and bool
               (= (count (lambda (x) (= x i)) digits)
                  (list-ref digits i)))))))
