#lang racket
(define point%
  (class* object% (writable<%>) (super-new) (init-field [x 0] [y 0])
    (define/public (copy) (new point% [x x] [y y]))
    (define/public (show) (format "<point% ~a ~a>" x y))
    (define/public (custom-write out) (write (show) out))
    (define/public (custom-display out) (display (show) out))))

(define circle%
  (class point% (super-new) (inherit-field x y) (init-field [r 0])
    (define/override (copy) (new circle% [x x] [y y] [r r]))
    (define/override (show) (format "<circle% ~a ~a>" (super show) r))
    (define/override (custom-write out) (write (show) out))
    (define/override (custom-display out) (display (show) out))))
