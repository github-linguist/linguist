(define ways-to-make-change
  (lambda (x coins)
    (cond
      [(null? coins) 0]
      [(< x 0) 0]
      [(zero? x) 1]
      [else (+ (ways-to-make-change x (cdr coins)) (ways-to-make-change (- x (car coins)) coins))])))

(ways-to-make-change 100)
