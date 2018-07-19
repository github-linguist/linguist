(define iota
  (lambda (count start step)
    (let loop ((result (list (+ start (* (- count 1) step)))))
      (let ((acc (car result)))
        (if (= acc start)
            result
            (loop (cons (- acc step) result)))))))


(define table
  (lambda (x)
    (let loop ((count 1)
               (numbers (iota x 1 1)))
      (if (not (null? numbers))
          (begin
            (display (make-string (* 6 (- count 1)) #\space))
            (for-each
             (lambda (n)
               (let ((number (number->string (* n count))))
                 (display (string-append
                           (make-string (- 6 (string-length number)) #\space)
                           number))))
             numbers)
            (newline)
            (loop (+ count 1)
                  (cdr numbers)))))))
