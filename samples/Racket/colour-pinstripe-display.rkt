#lang racket/gui

(define-values [W H] (get-display-size #t))

(define parts 4)
(define colors
  '("Black" "Red" "Green" "Blue" "Magenta" "Cyan" "Yellow" "White"))

(define (paint-pinstripe canvas dc)
  (send dc set-pen "black" 0 'transparent)
  (send dc set-brush "black" 'solid)
  (define H* (round (/ H parts)))
  (for ([row parts])
    (define Y (* row H*))
    (for ([X (in-range 0 W (add1 row))] [c (in-cycle colors)])
      (send dc set-brush c 'solid)
      (send dc draw-rectangle X Y (add1 row) H*))))

(define full-frame%
  (class frame%
    (define/override (on-subwindow-char r e)
      (when (eq? 'escape (send e get-key-code))
        (send this show #f)))
    (super-new
     [label "Color Pinstripe"] [width W] [height H]
     [style '(no-caption no-resize-border hide-menu-bar no-system-menu)])
    (define c (new canvas% [parent this] [paint-callback paint-pinstripe]))
    (send this show #t)))

(void (new full-frame%))
