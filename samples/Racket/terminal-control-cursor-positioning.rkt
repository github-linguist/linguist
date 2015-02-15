#lang racket
(require (planet neil/charterm:3:0))
(with-charterm
 (charterm-clear-screen)
 (charterm-cursor 3 6)
 (displayln "Hello World"))
