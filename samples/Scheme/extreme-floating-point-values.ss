(define infinity (/ 1.0 0.0))
(define minus-infinity (- infinity))
(define zero 0.0)
(define minus-zero (- zero))
(define not-a-number (/ 0.0 0.0))

(equal? (list infinity minus-infinity zero minus-zero not-a-number)
        (list   +inf.0         -inf.0  0.0       -0.0       +nan.0))
; #t
