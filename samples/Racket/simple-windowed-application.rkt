#lang racket/gui

(define frame (new frame% [label "There have been no clicks yet"]))

(define num-clicks 0)
(define (cb obj me)
  (set! num-clicks (add1 num-clicks))
  (send frame set-label (format "~a" num-clicks)))

(new button% [parent frame] [label "Click me"] [callback cb])
(send frame show #t)
