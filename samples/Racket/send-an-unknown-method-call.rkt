#lang racket
(define greeter
  (new (class object% (super-new)
         (define/public (hello name)
           (displayln (~a "Hello " name "."))))))

; normal method call
(send greeter hello "World")

; sending an unknown method
(define unknown 'hello)
(dynamic-send greeter unknown "World")
