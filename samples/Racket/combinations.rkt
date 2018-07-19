(define sublists
  (match-lambda**
   [(0 _)           '(())]
   [(_ '())         '()]
   [(m (cons x xs)) (append (map (curry cons x) (sublists (- m 1) xs))
                            (sublists m xs))]))

(define (combinations n m)
  (sublists n (range m)))
