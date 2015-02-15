(define (accumulator sum)
  (lambda (n)
    (set! sum (+ sum n))
    sum))

;; or:

(define ((accumulator sum) n)
  (set! sum (+ sum n))
  sum)

(define x (accumulator 1))
(x 5)
(display (accumulator 3)) (newline)
(display (x 2.3)) (newline)
