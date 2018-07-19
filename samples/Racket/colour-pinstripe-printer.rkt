#lang racket/gui

(define parts 4)

(define dc (new printer-dc%))
(send* dc (start-doc "Colour Pinstripe") (start-page))

(define-values [W H] (send dc get-size))
(define parts 4)
(define colors
  '("Black" "Red" "Green" "Blue" "Magenta" "Cyan" "Yellow" "White"))
(send dc set-pen "black" 0 'transparent)
(send dc set-brush "black" 'solid)
(define H* (round (/ H parts)))
(for ([row parts])
  (define Y (* row H*))
  (for ([X (in-range 0 W (add1 row))] [c (in-cycle colors)])
    (send dc set-brush c 'solid)
    (send dc draw-rectangle X Y (add1 row) H*)))

(send* dc (end-page) (end-doc))
