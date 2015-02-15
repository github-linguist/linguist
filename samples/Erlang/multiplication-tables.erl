-module( multiplication_tables ).

-export( [print_upto/1, task/0, upto/1] ).

print_upto( N ) ->
	Upto_tuples = [{X, {Y, Sum}} || {X, Y, Sum} <- upto(N)],
	io:fwrite( "  " ),
	[io:fwrite( "~5B", [X]) || X <- lists:seq(1, N)],
	io:nl(),
	io:nl(),
	[print_upto(X, proplists:get_all_values(X, Upto_tuples)) || X <- lists:seq(1, N)].
	

task() -> print_upto( 12 ).

upto( N ) -> [{X, Y, X*Y} || X <- lists:seq(1, N), Y <- lists:seq(1, N), Y >= X].



print_upto( N, Uptos ) ->
	io:fwrite( "~2B", [N] ),
	io:fwrite( "~*s", [5*(N - 1), " "] ),
	[io:fwrite("~5B", [Sum]) || {_Y, Sum} <- Uptos],
	io:nl().
