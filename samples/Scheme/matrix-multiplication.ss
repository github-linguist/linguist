(define (matrix-multiply matrix1 matrix2)
  (map
   (lambda (row)
    (apply map
     (lambda column
      (apply + (map * row column)))
     matrix2))
   matrix1))
