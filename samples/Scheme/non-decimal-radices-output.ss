(do ((i 0 (+ i 1)))
    ((>= i 33))
    (display (number->string i 2)) ; binary
    (display "  ")
    (display (number->string i 8)) ; octal
    (display "  ")
    (display (number->string i 10)) ; decimal, the "10" is optional
    (display "  ")
    (display (number->string i 16)) ; hex
    (newline))
