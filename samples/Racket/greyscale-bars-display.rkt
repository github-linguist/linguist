#lang racket/gui
(require slideshow/pict)

(define-values (*width* *height*) (values 400 40))

(define (shades inc)
  (for/list ([scale (in-range 0 (+ 1 inc) inc)])
    (round (* 255 scale))))

(define (grays increment direction)
  (define colors (shades increment))
  (apply hc-append
         ((if (eq? direction 'right) identity reverse)
          (for/list ([c colors])
            (colorize (filled-rectangle
                       (/ *width* (length colors)) *height*)
                      (make-color c c c))))))

(vc-append (grays 1/8 'right)  (grays 1/16 'left)
           (grays 1/32 'right) (grays 1/64 'left))
