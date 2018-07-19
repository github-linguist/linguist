(define (dot-product a b)
  (apply + (map * a b)))

(display (dot-product '(1 3 -5) '(4 -2 -1)))
(newline)
