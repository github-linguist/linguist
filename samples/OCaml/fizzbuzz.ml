let output x =
  match x mod 3 = 0, x mod 5 = 0 with
    true,  true  -> "FizzBuzz"
  | true,  false -> "Fizz"
  | false, true  -> "Buzz"
  | false, false -> string_of_int x

let _ =
  for i = 1 to 100 do print_endline (output i) done
