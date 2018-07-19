(define (carpet n)
  (define (in-carpet? x y)
    (cond ((or (zero? x) (zero? y))
              #t)
          ((and (= 1 (remainder x 3)) (= 1 (remainder y 3)))
              #f)
          (else
              (in-carpet? (quotient x 3) (quotient y 3)))))

  (do ((i 0 (+ i 1))) ((not (< i (expt 3 n))))
    (do ((j 0 (+ j 1))) ((not (< j (expt 3 n))))
      (display (if (in-carpet? i j)
                   #\*
                   #\space)))
    (newline)))
