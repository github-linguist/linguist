#lang racket

(require racket/draw)

;; flood-fill: bitmap<%> number number color color -> void
;; An example of flood filling a bitmap.
;;
;; We'll use a raw, byte-oriented interface here for demonstration
;; purposes.  Racket does provide get-pixel and set-pixel functions
;; which work on color% structures rather than bytes, but it's useful
;; to see that the byte approach works as well.
(define (flood-fill bm start-x start-y target-color replacement-color)
  ;; The main loop.
  ;; http://en.wikipedia.org/wiki/Flood_fill
  (define (iter x y)
    (when (and (in-bounds? x y) (target-color-at? x y))
      (replace-color-at! x y)
      (iter (add1 x) y)
      (iter (sub1 x) y)
      (iter x (add1 y))
      (iter x (sub1 y))))

  ;; With auxillary definitions below:
  (define width (send bm get-width))
  (define height (send bm get-height))

  (define buffer (make-bytes (* width height 4)))
  (send bm get-argb-pixels 0 0 width height buffer)

  (define-values (target-red target-green target-blue)
    (values (send target-color red)
            (send target-color green)
            (send target-color blue)))

  (define-values (replacement-red replacement-green replacement-blue)
    (values (send replacement-color red)
            (send replacement-color green)
            (send replacement-color blue)))

  (define (offset-at x y) (* 4 (+ (* y width) x)))

  (define (target-color-at? x y)
    (define offset (offset-at x y))
    (and (= (bytes-ref buffer (+ offset 1)) target-red)
         (= (bytes-ref buffer (+ offset 2)) target-green)
         (= (bytes-ref buffer (+ offset 3)) target-blue)))

  (define (replace-color-at! x y)
    (define offset (offset-at x y))
    (bytes-set! buffer (+ offset 1) replacement-red)
    (bytes-set! buffer (+ offset 2) replacement-green)
    (bytes-set! buffer (+ offset 3) replacement-blue))

  (define (in-bounds? x y)
    (and (<= 0 x) (< x width) (<= 0 y) (< y height)))

  ;; Finally, let's do the fill, and then store the
  ;; result back into the bitmap:
  (iter start-x start-y)
  (send bm set-argb-pixels 0 0 width height buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Example: flood fill a hole shape.
(define bm (make-bitmap 100 100))
(define dc (send bm make-dc))

;; We intentionally set the smoothing of the dc to
;; aligned so that there are no gaps in the shape for the
;; flood to leak through.
(send dc set-smoothing 'aligned)
(send dc draw-rectangle 10 10 80 80)
(send dc draw-rounded-rectangle 20 20 50 50)
;; In DrRacket, we can print the bm to look at it graphically,
;; before the flood fill:
bm

(flood-fill bm 50 50
            (send the-color-database find-color "white")
            (send the-color-database find-color "DarkSeaGreen"))
;; ... and after:
bm
