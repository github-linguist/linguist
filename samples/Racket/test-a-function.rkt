#lang racket
(module+ test (require rackunit))

;; from the Palindrome entry
(define (palindromb str)
  (let* ([lst (string->list (string-downcase str))]
         [slst (remove* '(#\space) lst)])
    (string=? (list->string (reverse slst)) (list->string slst))))

;; this test module is not loaded unless it is
;; specifically requested for testing, allowing internal
;; unit test specification
(module+ test
  (check-true (palindromb "racecar"))
  (check-true (palindromb "avoova"))
  (check-false (palindromb "potato")))
