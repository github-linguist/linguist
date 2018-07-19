#!r6rs

;; PS-TK example: display simple frame

(import (rnrs)
        (lib pstk main) ; change this to refer to your installation of PS/Tk
        )

(define tk (tk-start))
(tk/wm 'title tk "PS-Tk Example: Frame")

(tk-event-loop tk)
