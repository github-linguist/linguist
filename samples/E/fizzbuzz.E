for i in 1..100 {
   println(switch ([i % 3, i % 5]) {
     match [==0, ==0] { "FizzBuzz" }
     match [==0, _  ] { "Fizz" }
     match [_,   ==0] { "Buzz" }
     match _          { i }
   })
 }
