#lang racket

;; Explicit conversion of numbers can use the standard radices
(map (λ(r) (number->string 123 r)) '(2 8 10 16))
;; -> '("1111011" "173" "123" "7b")

;; There is also the `~r' formatting function that works with any radix
;; up to 36
(for/list ([r (in-range 2 37)]) (~r 123 #:base r))
;; -> '("1111011" "02111" "3231" "344" "323" "432" "173" "641" "123" "201"
;;      "3a" "69" "b8" "38" "7b" "47" "f6" "96" "36" "i5" "d5" "85" "35"
;;      "n4" "j4" "f4" "b4" "74" "34" "u3" "r3" "o3" "l3" "i3" "f3")
