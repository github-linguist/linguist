#lang racket

;; Using list operations
(define (stripchars1 text chars)
  (list->string (remove* (string->list chars) (string->list text))))

;; Using a regexp
;; => will be broken if chars have "-" or "]" or "\\"
(define (stripchars2 text chars)
  (regexp-replace* (~a "[" chars "]+") text ""))
