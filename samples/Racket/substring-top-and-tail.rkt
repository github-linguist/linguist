#lang racket

(define str "ストリング")

(substring str 1)
(substring str 0 (sub1 (string-length str)))
(substring str 1 (sub1 (string-length str)))
