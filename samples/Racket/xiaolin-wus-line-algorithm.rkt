#lang racket
(require 2htdp/image)

(define (plot img x y c)
  (define c*255 (exact-round (* (- 1 c) 255)))
  (place-image
   (rectangle 1 1 'solid (make-color c*255 c*255 c*255 255))
   x y img))

(define ipart exact-floor) ; assume that a "round-down" is what we want when -ve
;;; `round` is built in -- but we'll use exact round (and I'm not keen on over-binding round)

(define (fpart n) (- n (exact-floor n)))
(define (rfpart n) (- 1 (fpart n)))

(define (draw-line img x0 y0 x1 y1)
  (define (draw-line-steeped img x0 y0 x1 y1 steep?)
    (define (draw-line-steeped-l-to-r img x0 y0 x1 y1 steep?)
      (define dx (- x1 x0))
      (define dy (- y1 y0))
      (define gradient (/ dy dx))

      (define (handle-end-point img x y)
        (define xend (exact-round x))
        (define yend (+ y (* gradient (- xend x))))
        (define xgap (rfpart (+ x 0.5)))
        (define ypxl (ipart yend))
        (define intery (+ yend gradient))

        (case steep?
          [(#t)
           (define img* (plot img ypxl xend (* xgap (rfpart yend))))
           (values (plot img* (+ ypxl 1) xend (* xgap (fpart yend))) xend intery)]
          [(#f)
           (define img* (plot img xend ypxl (* xgap (rfpart yend))))
           (values (plot img* xend (+ ypxl 1) (* xgap (fpart yend))) xend intery)]))

      (define-values (img-with-l-endpoint xpl1 intery) (handle-end-point img x0 y0))
      (define-values (img-with-r-endpoint xpl2 _) (handle-end-point img-with-l-endpoint x1 y1))

      (for/fold ((img img-with-l-endpoint)  (y intery))
        ((x (in-range (+ xpl1 1) xpl2)))
        (define y-i (ipart y))
        (values
         (case steep?
           [(#t)
            (define img* (plot img y-i x (rfpart y)))
            (plot img* (+ 1 y-i) x (fpart y))]
           [(#f)
            (define img* (plot img x y-i (rfpart y)))
            (plot img* x (+ 1 y-i) (fpart y))])
         (+ y gradient))))

    (if (> x0 x1)
        (draw-line-steeped-l-to-r img x1 y1 x0 y0 steep?)
        (draw-line-steeped-l-to-r img x0 y0 x1 y1 steep?)))

  (define steep? (> (abs (- y1 y0)) (abs (- x1 x0))))
  (define-values (img* _)
    (if steep?
        (draw-line-steeped img y0 x0 y1 x1 steep?)
        (draw-line-steeped img x0 y0 x1 y1 steep?)))
  img*)

(define img-1
(beside
 (scale 3 (draw-line (empty-scene 150 100) 12 12 138 88))
 (above
  (scale 1 (draw-line (empty-scene 150 100) 12 50 138 50))
  (scale 1 (draw-line (empty-scene 150 100) 75 12 75 88))
  (scale 1 (draw-line (empty-scene 150 100) 12 88 138 12)))))

(define img-2
  (beside
 (scale 3 (draw-line (empty-scene 100 150) 12 12 88 138))
 (above (scale 1 (draw-line (empty-scene 100 150) 50 12 50 138))
        (scale 1 (draw-line (empty-scene 100 150) 12 75 88 75))
        (scale 1 (draw-line (empty-scene 100 150) 88 12 12 138)))))

img-1
img-2
(save-image img-1 "images/xiaolin-wu-racket-1.png")
(save-image img-2 "images/xiaolin-wu-racket-2.png")
