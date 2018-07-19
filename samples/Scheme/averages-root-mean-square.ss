(define (rms nums)
  (sqrt (/ (apply + (map * nums nums))
           (length nums))))

(rms '(1 2 3 4 5 6 7 8 9 10))
