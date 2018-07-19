(define non-squares
  (lambda (index)
    (+ index (inexact->exact (floor (+ (/ 1 2) (sqrt index)))))))

(define sequence
  (lambda (function)
    (lambda (start)
      (lambda (stop)
        (if (> start stop)
            (list)
            (cons (function start)
                  (((sequence function) (+ start 1)) stop)))))))

(define square?
  (lambda (number)
    ((lambda (root)
       (= (* root root) number))
     (floor (sqrt number)))))

(define any?
  (lambda (predicate?)
    (lambda (list)
      (and (not (null? list))
           (or (predicate? (car list))
               ((any? predicate?) (cdr list)))))))

(display (((sequence non-squares) 1) 22))
(newline)

(display ((any? square?) (((sequence non-squares) 1) 999999)))
(newline)
