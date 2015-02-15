(define (compose f g) (lambda (x) (f (g x))))
(define (cube x) (expt x 3))
(define (cube-root x) (expt x (/ 1 3)))

(define function (list sin cos cube))
(define inverse (list asin acos cube-root))

(define x 0.5)
(define (go f g)
  (if (not (or (null? f)
               (null? g)))
      (begin (display ((compose (car f) (car g)) x))
             (newline)
             (go (cdr f) (cdr g)))))

(go function inverse)
