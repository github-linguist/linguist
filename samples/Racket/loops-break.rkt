#lang racket
(let loop ()
  (let/ec break
    (define a (random 20))
    (displayln a)
    (when (= a 10) (break))
    (displayln (random 20))
    (loop)))
