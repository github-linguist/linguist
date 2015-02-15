#!/usr/local/bin/gosh

;; Show the first 20 niven numbers and the
;; first one greater than 1000.
(define (main args)
    (display (iota-filtered 20 1 niven?))(newline)
    (display (iota-filtered 1 1001 niven?))(newline))

;; Return a list of length n
;; for numbers starting at start
;; that satisfy the predicate fn.
(define (iota-filtered n start fn)
    (let loop ((num start)(lst (list)))
        (if (= (length lst) n)
            lst
            (loop (+ 1 num) (if (fn num) (append lst (list num)) lst)))))

;; Is a number a niven number?
(define (niven? n)
    (and (> n 0) (= 0 (remainder n (sum-of-digits n)))))

;; Get the sum of the digits of a number.
(define (sum-of-digits n)
    (apply + (map string->number (map string (string->list (number->string n))))))
