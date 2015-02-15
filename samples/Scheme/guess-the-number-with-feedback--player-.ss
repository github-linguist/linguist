(define minimum 1)
(define maximum 100)

(display "Enter a number from ")(display minimum)
(display " to ")(display maximum)(display ": ")
(define number (read))

(define input "")

(do ((guess (round (/ (+ maximum minimum) 2))  (round (/ (+ maximum minimum) 2))))
                ((string= input "="))
        (display "Is it ")(display guess)(display "?\n(h/l/=) > ")
        (set! input (symbol->string (read)))
        (if (string= input "h") (begin (display "OK...")
                (set! maximum (- guess 1))))
        (if (string= input "l") (begin (display "OK...")
                (set! minimum (+ guess 1)))))
(display "I was RIGHT!\n")
