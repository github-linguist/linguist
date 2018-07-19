#lang racket
(define (sierpinski n)
  (if (zero? n)
    '("*")
    (let ([spaces (make-string (expt 2 (sub1 n)) #\space)]
          [prev   (sierpinski (sub1 n))])
      (append (map (λ(x) (~a spaces x spaces)) prev)
              (map (λ(x) (~a x " " x)) prev)))))
(for-each displayln (sierpinski 5))
