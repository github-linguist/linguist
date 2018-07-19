(define-syntax sum
  (syntax-rules ()
    ((sum var low high . body)
     (let loop ((var low)
                (result 0))
       (if (> var high)
           result
           (loop (+ var 1)
                 (+ result . body)))))))
