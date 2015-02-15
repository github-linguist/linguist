;;; Direct translation of 'functional' ruby method
(define (powerset s)
  (for/fold ([outer-set (set(set))]) ([element s])
    (set-union outer-set
               (list->set (set-map outer-set
                                   (λ(inner-set) (set-add inner-set element)))))))
