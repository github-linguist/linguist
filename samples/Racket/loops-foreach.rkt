#lang racket

;; an example sequence
(define sequence '("something" 1 2 "foo"))

;; works for any sequence
(for ([i sequence])
  (displayln i))
