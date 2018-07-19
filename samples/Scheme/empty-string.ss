(define empty-string "")
(define (string-null? s) (string=? "" s))
(define (string-not-null? s) (string<? "" s))
