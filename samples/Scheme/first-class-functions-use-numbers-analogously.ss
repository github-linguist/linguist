(define x  2.0)
(define xi 0.5)
(define y  4.0)
(define yi 0.25)
(define z  (+ x y))
(define zi (/ (+ x y)))

(define number (list x y z))
(define inverse (list xi yi zi))

(define (multiplier n1 n2) (lambda (m) (* n1 n2 m)))

(define m 0.5)
(define (go n1 n2)
  (for-each (lambda (n1 n2)
              (display ((multiplier n1 n2) m))
              (newline))
            n1 n2))
(go number inverse)
