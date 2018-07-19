; in r5rs, there is append for lists, but we'll need to define vector-append
(define (vector-append . arg) (list->vector (apply append (map vector->list arg))))

(vector-append #(1 2 3 4) #(5 6 7) #(8 9 10))
; #(1 2 3 4 5 6 7 8 9 10)
