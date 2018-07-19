#lang racket
(for*/list ([x (in-range 1 21)]
            [y (in-range x 21)]
            [z (in-range y 21)]
            #:when (= (+ (* x x) (* y y)) (* z z)))
  (list x y z))
