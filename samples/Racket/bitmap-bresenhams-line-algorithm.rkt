#lang racket
(require racket/draw)

(define (draw-line dc x0 y0 x1 y1)
  (define dx (abs (- x1 x0)))
  (define dy (abs (- y1 y0)))
  (define sx (if (> x0 x1) -1 1))
  (define sy (if (> y0 y1) -1 1))
  (cond
    [(> dx dy)
     (let loop ([x x0] [y y0] [err (/ dx 2.0)])
       (unless (= x x1)
         (send dc draw-point x y)
         (define newerr (- err dy))
         (if (< newerr 0)
             (loop (+ x sx) (+ y sy) (+ newerr dx))
             (loop (+ x sx)    y        newerr))))]
    [else
     (let loop ([x x0] [y y0] [err (/ dy 2.0)])
       (unless (= y y1)
         (send dc draw-point x y)
         (define newerr (- err dy))
         (if (< newerr 0)
             (loop (+ x sx) (+ y sy)    newerr)
             (loop    x     (+ y sy) (+ newerr dy)))))]))

(define bm (make-object bitmap% 17 17))
(define dc (new bitmap-dc% [bitmap bm]))
(send dc set-smoothing 'unsmoothed)
(send dc set-pen "red" 1 'solid)
(for ([points '((1 8 8 16) (8 16 16 8) (16 8 8 1) (8 1 1 8))])
  (apply draw-line  (cons dc points)))
bm
