#lang racket/gui
(define-values [point _] (get-current-mouse-state))
(send point get-x)
(send point get-y)
