(define (forward-diff lst)
  (if (or (null? lst) (null? (cdr lst)))
      '()
      (cons (- (cadr lst) (car lst))
            (forward-diff (cdr lst)))))

(define (nth-forward-diff n xs)
  (if (= n 0)
      xs
      (nth-forward-diff (- n 1)
                        (forward-diff xs))))
