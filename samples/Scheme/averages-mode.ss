(define (mode collection)
    (define (helper collection counts)
        (if (null? collection)
            counts
            (helper (remove (car collection) collection)
                    (cons (cons (car collection)
                                (appearances (car collection) collection)) counts))))
    (map car
         (filter (lambda (x) (= (cdr x) (apply max (map cdr (helper collection '())))))
                 (helper collection '())))
