#lang racket

;; Using Racket's `string-trim'

(define str "  \n\t foo  bar   \r\n  ")

;; both sides:
(string-trim str) ; -> "foo  bar"

;; one side:
(string-trim str #:right? #f) ; -> "foo  bar   \r\n  "
(string-trim str #:left? #f)  ; -> "  \n\t foo  bar"

;; can also normalize spaces:
(string-normalize-spaces (string-trim str)) ; -> "foo bar"
