(define (fibo number)
  (define (fibo-rec number n i)
    (if (<= number 0)
        i
        (fibo-rec (- number 1) (+ n i) n)))
  (fibo-rec number 1 0))
