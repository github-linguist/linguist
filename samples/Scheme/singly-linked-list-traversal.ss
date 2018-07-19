(define (traverse seq func)
  (if (null? seq)
      '()
      (begin
        (func (car seq))
        (traverse (cdr seq) func))))
