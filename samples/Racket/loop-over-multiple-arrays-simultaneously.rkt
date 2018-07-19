#lang racket

(for ([i-1 '(a b c)]
      [i-2 '(A B C)]
      [i-3 '(1 2 3)])
  (printf "~a ~a ~a~n" i-1 i-2 i-3))
