(define (factors n)
  (define (*factors d)
    (cond ((> d n) (list))
          ((= (modulo n d) 0) (cons d (*factors (+ d 1))))
          (else (*factors (+ d 1)))))
  (*factors 1))

(display (factors 1111111))
(newline)
