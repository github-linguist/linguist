#lang racket
(require math)

(mean (in-range 0 1000)) ; -> 499 1/2
(mean '(2 2 4 4))        ; -> 3
(mean #(3 4 5 8))        ; -> 5
