-module( first_class_functions ).

-export( [task/0] ).

task() ->
	As = [fun math:sin/1, fun math:cos/1, fun cube/1],
	Bs = [fun math:asin/1, fun math:acos/1, fun square_inverse/1],
	[io:fwrite( "Value: 1.5 Result: ~p~n", [functional_composition([A, B], 1.5)]) || {A, B} <- lists:zip(As, Bs)].



functional_composition( Funs, X ) -> lists:foldl( fun(F, Acc) -> F(Acc) end, X, Funs ).

square( X ) -> math:pow( X, 2 ).

square_inverse( X ) -> math:sqrt( X ).
