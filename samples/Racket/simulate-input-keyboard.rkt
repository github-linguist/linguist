#lang racket/gui

(define frame (new frame%
                   (label "Example")
                   (width 300)
                   (height 300)))           ; Defines an instance of a frame to put the canvas in

(define simulate-key-canvas%
  (class canvas%
    (define/public (simulate-key key)
      (send this on-char key))              ; Creates a class that inherits from the standard canvas class, that can receive simulated key presses

    (define/override (on-char key)
      (displayln (send key get-key-code)))  ; Changes the method that receives key presses to show some output

    (super-new)))

(define canvas
  (new simulate-key-canvas%
       (parent frame)))                     ; Defines an instance of the newly created class

(send frame show #t)                        ; Shows the frame with a white canvas inside
(send canvas simulate-key (new key-event% (key-code #\k)))  ; Sends the simulated key press (with a key-event% instance)
;outputs k
