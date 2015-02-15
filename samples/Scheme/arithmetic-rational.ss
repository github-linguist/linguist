; simply prints all the perfect numbers
(do ((candidate 2 (+ candidate 1))) ((>= candidate (expt 2 19)))
  (let ((sum (/ 1 candidate)))
    (do ((factor 2 (+ factor 1))) ((>= factor (sqrt candidate)))
      (if (= 0 (modulo candidate factor))
          (set! sum (+ sum (/ 1 factor) (/ factor candidate)))))
    (if (= 1 (denominator sum))
        (begin (display candidate) (newline)))))
