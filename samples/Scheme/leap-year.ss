(define (leap-year? n)
(apply (lambda (a b c) (or a (and (not b) c)))
       (map (lambda (m) (zero? (remainder n m)))
            '(400 100 4))))
