-module( first_class_functions_use_numbers ).

-export( [task/0] ).

task() ->
	X = 2.0, Xi = 0.5, Y = 4.0, Yi = 0.25, Z = X + Y, Zi = 1.0 / (X + Y),
	As = [X, Y, Z],
	Bs = [Xi, Yi, Zi],
	[io:fwrite( "Value: 2.5 Result: ~p~n", [(multiplier(A, B))(2.5)]) || {A, B} <- lists:zip(As, Bs)].



multiplier( N1, N2 ) -> fun(M) -> N1 * N2 * M end.
