(define (my-compare a b)
  (cond ((< a b) "A is less than B")
        ((> a b) "A is greater than B")
        ((= a b) "A equals B")))

(my-compare (read) (read))
