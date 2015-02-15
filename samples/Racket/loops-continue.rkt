#lang racket

;; Idiomatic way
(for ([i (in-range 1 11)])
  (if (= (remainder i 5) 0)
      (printf "~a~n" i)
      (printf "~a, " i)))

;; Forces a skip, but not idiomatic because
;; the logic is less obvious
(for ([i (in-range 1 11)]
      #:unless (and (= (remainder i 5) 0)
                    (printf "~a~n" i)))
  (printf "~a, " i))
