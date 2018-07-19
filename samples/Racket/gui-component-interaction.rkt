#lang racket/gui

(define frame (new frame% [label "Interaction Demo"]))

(define inp
  (new text-field% [label "Value"] [parent frame] [init-value "0"]
       [callback
        (λ(f ev)
          (define v (send f get-value))
          (unless (string->number v)
            (send f set-value (regexp-replace* #rx"[^0-9]+" v ""))))]))

(define buttons (new horizontal-pane% [parent frame]))
(define inc-b
  (new button% [parent buttons] [label "Increment"]
       [callback (λ (b e) (let* ([v (string->number (send inp get-value))]
                                 [v (number->string (add1 v))])
                            (send inp set-value v)))]))
(define rand-b
  (new button% [parent buttons] [label "Random"]
       [callback (λ (b e) (when (message-box "Confirm" "Are you sure?"
                                             frame '(yes-no))
                            (send inp set-value (~a (random 10000)))))]))

(send frame show #t)
