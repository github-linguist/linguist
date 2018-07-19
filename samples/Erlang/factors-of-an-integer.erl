factors(N) ->
    [I || I <- lists:seq(1,trunc(N/2)), N rem I == 0]++[N].
