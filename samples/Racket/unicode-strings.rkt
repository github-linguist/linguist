#lang racket

;; Unicode in strings, using ascii
"\u03bb" ; -> "λ"
;; and since Racket source code is read in UTF-8, Unicode can be used
;; directly
"λ" ; -> same

;; The same holds for character constants
#\u3bb ; -> #\λ
#\λ    ; -> same

;; And of course Unicode can be used in identifiers,
(define √ sqrt)
(√ 256) ; -> 16
;; and in fact the standard language makes use of some of these
(λ(x) x) ; -> an identity function
