#lang racket
(require (planet neil/charterm:3:0))
(define x 0)
(define y 0)

(define (on-key k)
  (match k
    ['down   (move  0 -1)]
    ['up     (move  0 +1)]
    ['right  (move +1  0)]
    ['left   (move -1  0)]
    [else    #f]))

(define (move dx dy)
  (set! x (+ x dx))
  (set! y (+ y dy))
  (charterm-cursor x y))

(with-charterm
 (charterm-clear-screen)
 (charterm-cursor 0 0)
 (let loop ([continue? #t])
   (when continue?
     (loop (on-key (charterm-read-key))))))
