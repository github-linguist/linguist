#lang racket/gui

(define-values [W H] (get-display-size #t))

(define colors
  '("Black" "Red" "Green" "Blue" "Magenta" "Cyan" "Yellow" "White"))

(define (paint-pinstripe canvas dc)
  (send dc set-pen "black" 0 'transparent)
  (for ([x (in-range 0 W (/ W (length colors)))] [c colors])
    (send* dc (set-brush c 'solid) (draw-rectangle x 0 W H))))

(define full-frame%
  (class frame%
    (define/override (on-subwindow-char r e)
      (when (eq? 'escape (send e get-key-code))
        (send this show #f)))
    (super-new
     [label "Color bars"] [width W] [height H]
     [style '(no-caption no-resize-border hide-menu-bar no-system-menu)])
    (define c (new canvas% [parent this] [paint-callback paint-pinstripe]))
    (send this show #t)))

(void (new full-frame%))
