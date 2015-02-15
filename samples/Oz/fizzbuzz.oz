declare
  fun {FizzBuzz X}
     if     X mod 15 == 0 then 'FizzBuzz'
     elseif X mod  3 == 0 then 'Fizz'
     elseif X mod  5 == 0 then 'Buzz'
     else                      X
     end
  end
in
  for I in 1..100 do
     {Show {FizzBuzz I}}
  end
