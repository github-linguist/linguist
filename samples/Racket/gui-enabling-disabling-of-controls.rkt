#lang racket/gui

(define frame (new frame% [label "Interaction Demo"]))

(define (changed . _)
  (define s (send inp get-value))
  (define v (string->number s))
  (unless v (set! v (or (string->number (regexp-replace* #rx"[^0-9]+" s "")) 0))
            (send inp set-value (~a v)))
  (send inc-b enable (< v 10))
  (send dec-b enable (> v 0))
  (send inp enable (zero? v)))
(define ((change-value f) . _)
  (send inp set-value (number->string (f (string->number (send inp get-value)))))
  (changed))

(define inp
  (new text-field% [label "Value"] [parent frame] [init-value "0"] [callback changed]))

(define buttons (new horizontal-pane% [parent frame]))
(define inc-b
  (new button% [parent buttons] [label "Increment"] [callback (change-value add1)]))
(define dec-b
  (new button% [parent buttons] [label "Decrement"] [callback (change-value sub1)]))

(send frame show #t)
