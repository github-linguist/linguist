(require racket/match)

(define (sort pred l)
  (define (insert x y)
    (match (list x y)
      [(list x '()) (list x)]
      [(list x (cons y ys))
       (if (pred x y)
           (cons x (cons y ys))
           (cons y (f x ys)))]))
  (match l
    ['() '()]
    [(cons x xs) (insert x (sort pred xs))]))
