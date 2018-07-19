#lang racket

;; The racket/draw libraries provide imperative drawing functions.
;; http://docs.racket-lang.org/draw/index.html
(require racket/draw)

;; To create an image with width and height, use the make-bitmap
;; function.

;; For example, let's make a small image here:
(define bm (make-bitmap 640 480))

;; We use a drawing context handle, a "dc", to operate on the bitmap.
(define dc (send bm make-dc))

;; We can fill the bitmap with a color by using a combination of
;; setting the background, and clearing.
(send dc set-background (make-object color% 0 0 0)) ;; Color it black.
(send dc clear)

;; Let's set a few pixels to a greenish color with set-pixel:
(define aquamarine (send the-color-database find-color "aquamarine"))
(for ([i 480])
  (send dc set-pixel i i aquamarine))

;; We can get at the color of a bitmap pixel by using the get-pixel
;; method.  However, it may be faster to use get-argb-pixels if we
;; need a block of the pixels.  Let's use get-argb-pixels and look
;; at a row starting at (0, 42)
(define buffer (make-bytes (* 480 4)))  ;; alpha, red, green, blue
(send dc get-argb-pixels 0 42 480 1 buffer)

;; We can inspect the buffer
(bytes-ref buffer 0) ;;  and see that the first pixel's alpha is 255,
(bytes-ref buffer 1) ;;  and the red, green, and blue components are 0.
(bytes-ref buffer 2)
(bytes-ref buffer 3)

;; If we are using DrRacket, we can just print the bm as a toplevel expression
;; to view the final image:
bm
