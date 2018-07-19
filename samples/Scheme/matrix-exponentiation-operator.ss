(define (dec x)
  (- x 1))

(define (halve x)
  (/ x 2))

(define (row*col row col)
  (apply + (map * row col)))

(define (matrix-multiply m1 m2)
  (map
    (lambda (row)
      (apply map (lambda col (row*col row col))
        m2))
    m1))

(define (matrix-exp mat exp)
  (cond ((= exp 1) mat)
        ((even? exp) (square-matrix (matrix-exp mat (halve exp))))
        (else (matrix-multiply mat (matrix-exp mat (dec exp))))))

(define (square-matrix mat)
  (matrix-multiply mat mat))
