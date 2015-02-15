#lang racket

;; Number literals can use #x, #o, and #b for different radices
(list 123 #x7B #o173 #b1111011)
;; -> '(123 123 123 123)

;; Explicit conversion of strings can use any radix up to 16
(list (string->number     "123")
      (string->number     "123" 10)
      (string->number      "7B" 16)
      (string->number      "83" 15)
      (string->number      "96" 13)
      (string->number     "173"  8)
      (string->number   "11120"  3)
      (string->number "1111011"  2))
;; -> '(123 123 123 123 123 123 123 123)
