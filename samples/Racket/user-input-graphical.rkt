#lang racket
(require racket/gui)

(define str (get-text-from-user "Hi" "Enter a string"))
(message-box "Hi" (format "You entered: ~a" str))

(define n (get-text-from-user "Hi" "Enter a number"))
(message-box "Hi" (format "You entered: ~a"
                          (or (string->number n) "bogus text")))
