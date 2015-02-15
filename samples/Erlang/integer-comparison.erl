main() ->
    {ok, [N]} = io:fread("First integer: ", "~d"),
    {ok, [M]} = io:fread("First integer: ", "~d"),
    if
        N < M ->
            io:format("~b is less than ~b~n",[N,M]);
        N > M ->
            io:format("~b is greater than ~b~n",[N,M]);
        N == M ->
            io:format("~b is equal to ~b~n",[N,M])
    end.
