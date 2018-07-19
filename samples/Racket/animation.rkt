#lang racket/gui

;; One of 'left or 'right
(define direction 'left)

;; Set up the GUI
(define animation-frame%
  (class frame%
    (super-new [label "Animation"])
    ;; reverse direction on a click
    (define/override (on-subwindow-event win evt)
      (when (send evt button-down?)
        (set! direction
              (if (eq? direction 'left)
                  'right
                  'left))))))

(define frame (new animation-frame%))
(define msg (new message%
                 [label "Hello World! "]
                 [parent frame]))

;; Timer callback to adjust the message
(define (callback)
  (define old-label (send msg get-label))
  (define len (string-length old-label))
  (if (eq? direction 'left)
      (send msg set-label
            (string-append (substring old-label 1)
                           (substring old-label 0 1)))
      (send msg set-label
            (string-append (substring old-label (- len 1) len)
                           (substring old-label 0 (- len 1))))))

;; Set a timer and go
(define timer (new timer%
                   [notify-callback callback]
                   [interval 500]))

(send frame show #t)
