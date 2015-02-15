(define (eval-with-x prog a b)
  (let ((at-a (eval `(let ((x ',a)) ,prog)))
        (at-b (eval `(let ((x ',b)) ,prog))))
    (- at-b at-a)))
