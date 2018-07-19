#lang racket
(define (sing bottles)
  (define (plural n) (~a n " bottle" (if (= n 1) "" "s")))
  (printf "~a of beer on the wall\n~a of beer\n~
           Take one down, pass it around\n~a of beer on the wall\n\n"
          (plural bottles) (plural bottles) (plural (sub1 bottles)))
  (unless (= 1 bottles) (sing (sub1 bottles))))
(sing 100)
