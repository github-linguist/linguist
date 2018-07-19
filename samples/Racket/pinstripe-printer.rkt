#lang racket/gui

(define parts 4)

(define dc (new printer-dc%))
(send* dc (start-doc "Pinstripe") (start-page))

(define-values [W H] (send dc get-size))
(send dc set-pen "black" 0 'solid)
(send dc set-brush "black" 'solid)
(define H* (round (/ H parts)))
(for ([row parts])
  (define Y (* row H*))
  (for ([X (in-range 0 W (* (add1 row) 2))])
    (send dc draw-rectangle X Y (add1 row) H*)))

(send* dc (end-page) (end-doc))
