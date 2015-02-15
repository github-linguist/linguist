#lang racket/gui
(send (new frame%
           [label "New Window"]
           [width 100] [height 100])
      show #t)
