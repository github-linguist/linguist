#lang racket
(define (converter temp init final)
  (define to-k
    (case init
      ('k temp)
      ('c (+ 273.15 temp))
      ('f (* (+ temp 459.67) 5/9))
      ('r (* temp 5/9))))
  (case final
    ('k to-k)
    ('c (- to-k 273.15))
    ('f (- (* to-k 9/5) 459.67))
    ('r (* to-k 1.8))))

(define (kelvin-to-all temp)
  (display (format "Kelvin: ~a \nCelsius: ~a \nFahrenheit: ~a \nRankine: ~a \n"
                   temp
                   (converter temp 'k 'c)
                   (converter temp 'k 'f)
                   (converter temp 'k 'r))))
(kelvin-to-all 21)
;Kelvin: 21
;Celsius: -252.14999999999998
;Fahrenheit: -421.87
;Rankine: 37.800000000000004
