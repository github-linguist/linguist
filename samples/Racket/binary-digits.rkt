#lang racket
;; Option 1: binary formatter
(for ([i 16]) (printf "~b\n" i))
;; Option 2: explicit conversion
(for ([i 16]) (displayln (number->string i 2)))
