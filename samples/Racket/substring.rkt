#lang racket

(define str "abcdefghijklmnopqrstuvwxyz")

(define n 10)
(define m 2)
(define start-char #\x)
(define start-str "xy")

;; starting from n characters in and of m length;
(substring str n (+ n m)) ; -> "kl"

;; starting from n characters in, up to the end of the string;
(substring str m) ; -> "klmnopqrstuvwxyz"

;; whole string minus last character;
(substring str 0 (sub1 (string-length str))) ; -> "abcdefghijklmnopqrstuvwxy"

;; starting from a known character within the string and of m length;
(substring str (caar (regexp-match-positions (regexp-quote (string start-char))
                                             str))) ; -> "xyz"

;; starting from a known substring within the string and of m length.
(substring str (caar (regexp-match-positions (regexp-quote start-str)
                                             str))) ; -> "xyz"
