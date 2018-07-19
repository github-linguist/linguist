(define pi (* 4 (atan 1)))

(define radians (/ pi 4))
(define degrees 45)

(display (sin radians))
(display " ")
(display (sin (* degrees (/ pi 180))))
(newline)

(display (cos radians))
(display " ")
(display (cos (* degrees (/ pi 180))))
(newline)

(display (tan radians))
(display " ")
(display (tan (* degrees (/ pi 180))))
(newline)

(define arcsin (asin (sin radians)))
(display arcsin)
(display " ")
(display (* arcsin (/ 180 pi)))
(newline)

(define arccos (acos (cos radians)))
(display arccos)
(display " ")
(display (* arccos (/ 180 pi)))
(newline)

(define arctan (atan (tan radians)))
(display arctan)
(display " ")
(display (* arctan (/ 180 pi)))
(newline)
