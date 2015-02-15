#lang racket

(define (menu choices)
  (cond [(null? choices) ""]
        [else (for ([c choices] [i (in-naturals 1)]) (printf "~a. ~a\n" i c))
              (printf "Enter a number: ")
              (define n (string->number (read-line)))
              (or (and (exact-integer? n)
                       (<= 1 n (length choices))
                       (list-ref choices (sub1 n)))
                  (menu choices))]))

(menu '("fee fie" "huff and puff" "mirror mirror" "tick tock"))
