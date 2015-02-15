#lang racket
(define (carpet n)
  (if (zero? n)
    '("#")
    (let* ([prev   (carpet (sub1 n))]
           [spaces (regexp-replace* #rx"#" (car prev) " ")])
      (append (map (λ(x) (~a x x x)) prev)
              (map (λ(x) (~a x spaces x)) prev)
              (map (λ(x) (~a x x x)) prev)))))
(for-each displayln (carpet 3))
