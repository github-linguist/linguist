#lang racket
(for ((i (in-range 1 15)))
  (display i)
  #:break (= 10 i)
  (display ", "))
