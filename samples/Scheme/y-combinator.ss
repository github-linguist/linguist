(define Y
  (lambda (f)
    ((lambda (x) (x x))
     (lambda (g)
       (f (lambda args (apply (g g) args)))))))

(define fac
  (Y
    (lambda (f)
      (lambda (x)
        (if (< x 2)
            1
            (* x (f (- x 1))))))))

(define fib
  (Y
    (lambda (f)
      (lambda (x)
        (if (< x 2)
            x
            (+ (f (- x 1)) (f (- x 2))))))))

(display (fac 6))
(newline)

(display (fib 6))
(newline)
