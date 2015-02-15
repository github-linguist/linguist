#lang racket

(require racket/draw)

(define (iterations a z i)
  (define z′ (+ (* z z) a))
  (if (or (= i 255) (> (magnitude z′) 2))
      i
      (iterations a z′ (add1 i))))

(define (iter->color i)
  (if (= i 255)
      (make-object color% "black")
      (make-object color% (* 5 (modulo i 15)) (* 32 (modulo i 7)) (* 8 (modulo i 31)))))

(define (mandelbrot width height)
  (define target (make-bitmap width height))
  (define dc (new bitmap-dc% [bitmap target]))
  (for* ([x width] [y height])
    (define real-x (- (* 3.0 (/ x width)) 2.25))
    (define real-y (- (* 2.5 (/ y height)) 1.25))
    (send dc set-pen (iter->color (iterations (make-rectangular real-x real-y) 0 0)) 1 'solid)
    (send dc draw-point x y))
  (send target save-file "mandelbrot.png" 'png))

(mandelbrot 300 200)
