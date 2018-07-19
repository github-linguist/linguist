(do ((i 1 (+ i 1)))
    ((> i 100))
    (display
      (cond ((= 0 (modulo i 15)) "FizzBuzz")
            ((= 0 (modulo i 3))  "Fizz")
            ((= 0 (modulo i 5))  "Buzz")
            (else                i)))
    (newline))
