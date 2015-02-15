(define (step-up n-steps)
  (cond ((zero? n-steps) 'done)
        ((step) (step-up (- n-steps 1)))
        (else (step-up (+ n-steps 1)))))
