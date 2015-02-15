(define (day-of-week year month day)
(if (< month 3)
    (begin (set! month (+ month 12)) (set! year (- year 1))))
(+ 1
   (remainder (+ 5 day (quotient (* (+ 1 month) 13) 5)
                 year (quotient year 4) (* (quotient year 100) 6) (quotient year 400))
              7)))

(define (task)
(let loop ((y 2121) (v '()))
(if (< y 2008)
    v
    (loop (- y 1)
          (if (= 7 (day-of-week y 12 25))
              (cons y v)
              v)))))

(task)
; (2011 2016 2022 2033 2039 2044 2050 2061 2067 2072 2078 2089 2095 2101 2107 2112 2118)
