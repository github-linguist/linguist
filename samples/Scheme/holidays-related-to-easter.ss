; Easter sunday. Result is a list '(month day)
;
; See:
; Jean Meeus, "Astronomical Formulae for Calculators",
; 4th edition, Willmann-Bell, 1988, p.31

(define (easter year)
        (let* ((a (remainder year 19))
               (b (quotient year 100))
               (c (remainder year 100))
               (d (quotient b 4))
               (e (remainder b 4))
               (f (quotient (+ b 8) 25))
               (g (quotient (+ 1 (- b f)) 3))
               (h (remainder (+ (* 19 a) (- b d g) 15) 30))
               (i (quotient c 4))
               (k (remainder c 4))
               (l (remainder (+ e e i i (- 32 h k)) 7))
               (m (quotient (+ a (* 11 h) (* 22 l)) 451))
               (n (+ h l (- 114 (* 7 m)))))
              (list (quotient n 31) (+ 1 (remainder n 31)))))
