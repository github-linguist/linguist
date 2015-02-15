#lang racket
(require racket/draw)

(define (draw-line dc p q)
  (match* (p q) [((list x y) (list s t)) (send dc draw-line x y s t)]))

(define (draw-lines dc ps)
  (void
   (for/fold ([p0 (first ps)]) ([p (rest ps)])
     (draw-line dc p0 p)
     p)))

(define (int t p q)
  (define ((int1 t) x0 x1) (+ (* (- 1 t) x0) (* t x1)))
  (map (int1 t) p q))

(define (bezier-points p0 p1 p2)
  (for/list ([t (in-range 0.0 1.0 (/ 1.0 20))])
    (int t (int t p0 p1) (int t p1 p2))))

(define bm (make-object bitmap% 17 17))
(define dc (new bitmap-dc% [bitmap bm]))
(send dc set-smoothing 'unsmoothed)
(send dc set-pen "red" 1 'solid)
(draw-lines dc (bezier-points '(16 1) '(1 4) '(3 16)))
bm
