(define (make-stack)
  (let ((st '()))
    (lambda (message . args)
      (case message
        ((empty?) (null? st))
        ((top) (if (null? st)
                   'empty
                   (car st)))
        ((push) (set! st (cons (car args) st)))
        ((pop) (if (null? st)
                   'empty
                   (let ((result (car st)))
                     (set! st (cdr st))
                     result)))
        (else 'badmsg)))))
