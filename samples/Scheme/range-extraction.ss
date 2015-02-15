(define (make-range start end)
  (cond ((= start end)
         `("," ,start))
        ((= end (+ start 1))
         `("," ,start "," ,end))
        (else
         `("," ,start "-" ,end))))

(define (range-extract-0 start end a)
  (cond ((null? a)
         (make-range start end))
        ((= (+ 1 end) (car a))
         (range-extract-0 start (car a) (cdr a)))
        (else
         (append (make-range start end)
                 (range-extract-0 (car a) (car a) (cdr a))))))

(define (range-extract a)
  (apply string-append (map (lambda (x)
                              (if (number? x)
                                  (number->string x)
                                  x))
                            (cdr (range-extract-0 (car a) (car a) (cdr a))))))

(range-extract '( 0  1  2  4  6  7  8 11 12 14
                 15 16 17 18 19 20 21 22 23 24
                 25 27 28 29 30 31 32 33 35 36
                 37 38 39))
