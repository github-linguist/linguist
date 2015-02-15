(define apple 0)
(define banana 1)
(define cherry 2)

(define (fruit? atom)
  (or (equal? 'apple atom)
      (equal? 'banana atom)
      (equal? 'cherry atom)))
