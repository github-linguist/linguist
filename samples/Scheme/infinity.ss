+inf.0 ; positive infinity
(define (finite? x) (< -inf.0 x +inf.0))
(define (infinite? x) (not (finite? x)))
