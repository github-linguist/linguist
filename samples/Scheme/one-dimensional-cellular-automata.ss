(define (next-generation left petri-dish right)
  (if (null? petri-dish)
      (list)
      (cons (if (= (+ left
                      (car petri-dish)
                      (if (null? (cdr petri-dish))
                          right
                          (cadr petri-dish)))
                   2)
                1
                0)
            (next-generation (car petri-dish) (cdr petri-dish) right))))

(define (display-evolution petri-dish generations)
  (if (not (zero? generations))
      (begin (display petri-dish)
             (newline)
             (display-evolution (next-generation 0 petri-dish 0)
                                (- generations 1)))))

(display-evolution (list 1 1 1 0 1 1 0 1 0 1 0 1 0 1 0 0 1 0) 10)
