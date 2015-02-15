(define (mean l)
  (if (null? l)
      0
      (/ (apply + l) (length l))))
