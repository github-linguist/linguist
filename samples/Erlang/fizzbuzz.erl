fizzbuzz() ->
    F = fun(N) when N rem 15 == 0 -> "FizzBuzz";
           (N) when N rem 3 == 0  -> "Fizz";
           (N) when N rem 5 == 0  -> "Buzz";
           (N) -> integer_to_list(N)
        end,
    [F(N)++"\n" || N <- lists:seq(1,100)].
