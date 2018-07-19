#lang racket/gui

(define (say . xs) (printf ">>> ~a\n" (apply ~a xs)) (flush-output))

(define frame (new frame% [label "Demo"] [width 400] [height 400]))
(say "frame = " frame) ; plain value

(say 'Show)     (send frame show #t)      (sleep 1)
(say 'Hide)     (send frame show #f)      (sleep 1)
(say 'Show)     (send frame show #t)      (sleep 1)
(say 'Minimize) (send frame iconize #t)   (sleep 1)
(say 'Restore)  (send frame iconize #f)   (sleep 1)
(say 'Maximize) (send frame maximize #t)  (sleep 1)
(say 'Restore)  (send frame maximize #f)  (sleep 1)
(say 'Move)     (send frame move 100 100) (sleep 1)
(say 'Resize)   (send frame resize 100 100) (sleep 1)
(say 'Close)    (send frame show #f) (sleep 1) ; that's how we close a window
