#lang racket
(define hello "hello")
(displayln hello)

(define world (string-append hello " " "world" "!"))
(displayln world)

;outputs:
;  hello
;  hello world!
