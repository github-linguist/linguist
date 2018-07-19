  #lang racket/base
  (define (add f g x)
    (+ (f x) (g x)))
  (add sin cos 10)
