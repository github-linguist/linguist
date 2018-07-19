#lang racket/gui

(define frame (new frame%
                   [label "Example"]
                   [width 300]
                   [height 300]))
(new canvas% [parent frame]
             [paint-callback
              (lambda (canvas dc)
                (send dc set-scale 3 3)
                (send dc set-text-foreground "blue")
                (send dc draw-text "Don't Panic!" 0 0))])
(send frame show #t)
