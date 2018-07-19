;;;The heart of the algorithm
(define (S n)
  (let ((m (- (expt 2 n) 1)))
    (let loop ((c (- n 2)) (a 4))
      (if (zero? c)
          a
          (loop (- c 1) (remainder (- (* a a) 2) m))))))

(define (mersenne-prime? n)
  (if (= n 2)
    #t
    (zero? (S n))))

;;;Trivial unoptimized implementation for the base primes
(define (next-prime x)
  (if (prime? (+ x 1))
      (+ x 1)
      (next-prime (+ x 1))))

(define (prime? x)
  (let loop ((c 2))
    (cond ((>= c x) #t)
          ((zero? (remainder x c)) #f)
          (else (loop (+ c 1))))))

;;Main loop
(let loop ((i 45) (p 2))
  (if (not (zero? i))
      (if (mersenne-prime? p)
          (begin
            (display "M") (display p) (display " ")
            (loop (- i 1) (next-prime p)))
          (loop i (next-prime p)))))
