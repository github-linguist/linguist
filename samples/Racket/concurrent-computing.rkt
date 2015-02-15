#lang racket
(for ([str '("Enjoy" "Rosetta" "Code")])
  (thread (λ () (displayln str))))
