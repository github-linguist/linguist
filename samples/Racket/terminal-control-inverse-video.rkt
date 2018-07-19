#lang racket
(require (planet neil/charterm:3:0))

(with-charterm
 (charterm-clear-screen)
 (charterm-cursor 0 0)
 (charterm-inverse)
 (charterm-display "Hello")
 (charterm-normal)
 (charterm-display "World"))
