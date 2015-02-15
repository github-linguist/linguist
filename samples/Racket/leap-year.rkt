(define (leap-year? y)
  (and (zero? (modulo y 4)) (or (positive? (modulo y 100)) (zero? (modulo y 400)))))
