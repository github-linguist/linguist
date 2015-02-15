#lang racket
(require racket/draw)

(define (percentage-difference bitmap1 bitmap2)
  (define width (send bitmap1 get-width))
  (define height (send bitmap1 get-height))
  (define buffer1 (make-bytes (* width height 4)))
  (define buffer2 (make-bytes (* width height 4)))
  (send (send bitmap1 make-dc) get-argb-pixels 0 0 width height buffer1)
  (send (send bitmap2 make-dc) get-argb-pixels 0 0 width height buffer2)
  (/ (* 100.0
        (for/fold ((difference 0))
          ((i (in-naturals)) (x1 (in-bytes buffer1)) (x2 (in-bytes buffer2)))
          (if (zero? (remainder i 4))
              difference
              (+ difference (abs (- x1 x2))))))
     width height 3 256))

(define lenna50 (read-bitmap "lenna50.jpg"))
(define lenna100 (read-bitmap "lenna100.jpg"))

(percentage-difference lenna50 lenna100) ;-> 1.7749329408009846
