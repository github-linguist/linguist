#lang racket
(require slideshow/pict)

(define (yin-yang d)
  (define base
    (hc-append (inset/clip (circle d) 0 0 (- (/ d 2)) 0)
               (inset/clip (disk d) (- (/ d 2)) 0 0 0)))
  (define with-top
    (ct-superimpose
     base
     (cc-superimpose (colorize (disk (/ d 2)) "white")
                     (disk (/ d 8)))))
  (define with-bottom
    (cb-superimpose
     with-top
     (cc-superimpose (disk (/ d 2))
                     (colorize (disk (/ d 8)) "white"))))
  (cc-superimpose with-bottom (circle d)))

(yin-yang 200)
