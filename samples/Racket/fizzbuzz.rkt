(for ([n (in-range 1 101)])
  (displayln
   (match (gcd n 15)
     [15 "fizzbuzz"]
     [3 "fizz"]
     [5 "buzz"]
     [_ n])))
