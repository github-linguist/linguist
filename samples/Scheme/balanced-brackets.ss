(define (balanced-brackets string)
  (define (b chars sum)
    (cond ((and (null? chars) (= 0 sum))
           #t)
          ((null? chars)
           #f)
          ((char=? #\[ (car chars))
           (b (cdr chars) (+ sum 1)))
          ((= sum 0)
           #f)
          (else
           (b (cdr chars) (- sum 1)))))
  (b (string->list string) 0))

(balanced-brackets "")

(balanced-brackets "[]")
(balanced-brackets "[][]")
(balanced-brackets "[[][]]")

(balanced-brackets "][")
(balanced-brackets "][][")
(balanced-brackets "[]][[]")
