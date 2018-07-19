#lang racket
(require 2htdp/image 2htdp/universe)

(define black (color   0   0   0 255))
(define white (color 255 255 255 255))

(define-struct world (last fps))

(define (noise w h)
  (color-list->bitmap
   (for*/list ([x (in-range w)] [y (in-range h)])
     (if (zero? (random 2)) black white))
   w h))

(define (draw w)
  (underlay/xy
   (noise 320 240) 0 0
   (text (number->string (world-fps w)) 64 "Red")))

(define (handle-tick w)
  (define cm (current-inexact-milliseconds))
  (make-world cm (exact-floor (/ 1000.0 (- cm (world-last w))))))

(big-bang (make-world 1 0)
          [on-draw draw]
          [on-tick handle-tick (/ 1. 120)])
