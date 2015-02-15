#lang racket

(with-handlers ([exn:fail:contract:divide-by-zero?
                 (λ (e) (displayln "Divided by zero"))])
  (/ 1 0))
