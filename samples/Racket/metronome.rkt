#lang racket

(require racket/gui)

(define msec 500)
(define sounds '("hi.wav" "lo.wav"))
(define colors '("red" "green"))

(define f
  (new frame% [label "Metronome"] [width 200] [height 200]))
(define c
  (new (class canvas%
         (define brushes
           (map (λ(c) (new brush% [color c] [style 'solid])) colors))
         (define cur 0)
         (define/override (on-paint)
           (send* (send this get-dc)
                  (clear)
                  (set-brush (list-ref brushes cur))
                  (draw-rectangle 0 0 200 200)))
         (define/public (flip!)
           (set! cur (modulo (add1 cur) (length sounds)))
           (play-sound (list-ref sounds cur) #f)
           (on-paint))
         (super-new))
       [parent f]))

(define (flip)
  (define init (current-inexact-milliseconds))
  (define next (+ msec init))
  (define ticks 1)
  (let loop ()
    (when (> (current-inexact-milliseconds) next)
      (set! ticks (add1 ticks))
      (set! next (+ init (* msec ticks)))
      (queue-callback (λ() (send c flip!))))
    (sleep 0.01)
    (loop)))

(send* f (center) (show #t))
(void (thread flip))
