#lang racket
(require images/flomap math)

(define lena <<paste image of Lena here>> )
(define bm (send lena get-bitmap))
(define fm (bitmap->flomap bm))

(flomap->bitmap
 (build-flomap
  4 (send bm get-width) (send bm get-height)
  (λ (k x y)
    (define (f x y) (flomap-ref fm k x y))
    (median < (list (f (- x 1) (- y 1))
                    (f (- x 1)    y)
                    (f (- x 1) (+ y 1))
                    (f    x    (- y 1))
                    (f    x       y)
                    (f    x    (+ y 1))
                    (f (+ x 1) (- y 1))
                    (f (+ x 1)    y)
                    (f (+ x 1) (+ y 1)))))))
