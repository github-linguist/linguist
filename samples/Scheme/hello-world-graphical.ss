#!r6rs

;; PS-TK example: display frame + label

(import (rnrs)
        (lib pstk main) ; change this to refer to your PS/Tk installation
        )

(define tk (tk-start))
(tk/wm 'title tk "PS-Tk Example: Label")

(let ((label (tk 'create-widget 'label 'text: "Goodbye, world")))
  (tk/place label 'height: 20 'width: 50 'x: 10 'y: 20))

(tk-event-loop tk)
