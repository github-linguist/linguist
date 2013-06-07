; Clean, simple and efficient code -- that's the power of Racket!
; http://racket-lang.org/

(define (bottles n more)
  (printf "~a bottle~a of beer~a"
          (case n [(0) "no more"] [(1) "1"] [else n])
          (if (= n 1) "" "s")
          more))

(for ([n (in-range 99 0 -1)])
  (bottles n " on the wall, ")
  (bottles n ".\n")
  (printf "Take one down and pass it around, ")
  (bottles (sub1 n) " on the wall.\n\n"))

(displayln "No more bottles of beer on the wall, no more bottles of beer.")
(displayln "Go to the store and buy some more, 99 bottles of beer on the wall.")
