#lang racket

;; Byte strings can be created either by a function (b1)
;; or as a literal string (b2). No operation is needed for
;; destruction due to garbage collection.

(define b1 (make-bytes 5 65))  ; b1 -> #"AAAAA"
(define b2 #"BBBBB")           ; b2 -> #"BBBBB"

;; String assignment. Note that b2 cannot be
;; mutated since literal byte strings are immutable.

(bytes-set! b1 0 66)           ; b1 -> #"BAAAA"

;; Comparison. Less than & greater than are
;; lexicographic comparison.

(bytes=? b1 b2)                ; -> #f
(bytes<? b1 b2)                ; -> #t
(bytes>? b1 b2)                ; -> #f

;; Byte strings can be cloned by copying to a
;; new one or by overwriting an existing one.

(define b3 (bytes-copy b1))    ; b3 -> #"BAAAA"
(bytes-copy! b1 0 b2)          ; b1 -> #"BBBBB"

;; Byte strings can be appended to one another. A
;; single byte is appended as a length 1 string.

(bytes-append b1 b2)           ; -> #"BBBBBBBBBB"
(bytes-append b3 #"B")         ; -> #"BAAAAB"

;; Substring

(subbytes b3 0)                ; -> #"BAAAA"
(subbytes b3 0 2)              ; -> #"BA"

;; Regular expressions can be used to do replacements
;; in a byte string (or ordinary strings)

(regexp-replace #"B" b1 #"A")  ; -> #"ABBBB" (only the first one)
(regexp-replace* #"B" b1 #"A") ; -> #"AAAAA"

;; Joining strings

(bytes-join (list b2 b3) #" ") ; -> #"BBBBB BAAAA"
