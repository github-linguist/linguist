(define (sierpinski n)
  (for-each
   (lambda (x) (display (list->string x)) (newline))
   (let loop ((acc (list (list #\*))) (spaces (list #\ )) (n n))
     (if (zero? n)
         acc
         (loop
          (append
           (map (lambda (x) (append spaces x spaces)) acc)
           (map (lambda (x) (append x (list #\ ) x)) acc))
          (append spaces spaces)
          (- n 1))))))
