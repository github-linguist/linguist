(define (perf n)
  (let loop ((i 1)
             (sum 0))
    (cond ((= i n)
           (= sum n))
          ((= 0 (modulo n i))
           (loop (+ i 1) (+ sum i)))
          (else
           (loop (+ i 1) sum)))))
