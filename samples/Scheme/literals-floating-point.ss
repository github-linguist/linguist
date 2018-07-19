.2      ; 0.2
2.      ; 2.0
2e3     ; 2000
2.+3.i  ; complex floating-point number

; in Scheme, floating-point numbers are inexact numbers
(inexact? 2.)
; #t
(inexact? 2)
; #f
