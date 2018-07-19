(define (fibonacci n)
  (if (> 0 n)
      "Error: argument must not be negative."
      (let aux ((a 1) (b 0) (count n))
        (if (= count 0)
            b
            (aux (+ a b) a (- count 1))))))

(map fibonacci '(1 2 3 4 5 6 7 8 9 10))
