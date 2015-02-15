 #lang racket/gui
(require racket/gui/base)

; Make a frame by instantiating the frame% class
(define frame (new frame% [label "Goodbye, World!"]))

; Make a static text message in the frame
(define msg (new message% [parent frame]
                          [label "No events so far..."]))

; Make a button in the frame
(new button% [parent frame]
             [label "Click Me"]
             ; Callback procedure for a button click:
             (callback (lambda (button event)
                         (send msg set-label "Button click"))))

; Show the frame by calling its show method
(send frame show #t)
