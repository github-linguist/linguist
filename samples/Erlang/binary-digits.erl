lists:map( fun(N) -> io:fwrite("~.2B~n", [N]) end, [5, 50, 9000]).
