#lang racket

(define xs '(1.0 2.0 3.0 1.0e11))
(define ys '(1.0 1.4142135623730951 1.7320508075688772 316227.76601683791))

(define xprecision 3)
(define yprecision 5)

(with-output-to-file "some-file" #:exists 'truncate
  (λ() (for ([x xs] [y ys])
         (displayln (~a (~r x #:precision xprecision)
                    "  "
                    (~r y #:precision yprecision))))))

#|
The output is not using exponenets as above, but that's not needed
since Racket can read these numbers fine:

1  1
2  1.41421
3  1.73205
100000000000  316227.76602
|#
