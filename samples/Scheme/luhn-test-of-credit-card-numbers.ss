(define luhn
  (lambda (n)
    (let loop ((number n)
               (index 0)
               (result 0))
      (if (= 0 number)
          (= 0 (remainder result 10))
          (loop (quotient number 10)
                (+ index 1)
                (+ result
                   (if (even? index)
                       (remainder number 10)
                       (let ((part (* 2 (remainder number 10))))
                         (+ (remainder part 10) (quotient part 10))))))))))
