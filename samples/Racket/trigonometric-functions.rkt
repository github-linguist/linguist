#lang racket
(define radians (/ pi 4))
(define degrees 45)

(displayln (format "~a ~a" (sin radians) (sin (* degrees (/ pi 180)))))

(displayln (format "~a ~a" (cos radians) (cos (* degrees (/ pi 180)))))

(displayln (format "~a ~a" (tan radians) (tan (* degrees (/ pi 180)))))

(define arcsin (asin (sin radians)))
(displayln (format "~a ~a" arcsin (* arcsin (/ 180 pi))))

(define arccos (acos (cos radians)))
(displayln (format "~a ~a" arccos (* arccos (/ 180 pi))))

(define arctan (atan (tan radians)))
(display (format "~a ~a" arctan (* arctan (/ 180 pi))))
