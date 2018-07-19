-module( van_der_corput ).

-export( [sequence/1, sequence/2, task/0] ).

sequence( N ) -> sequence( N, 2 ).

sequence( 0, _Base ) -> 0.0;
sequence( N, Base ) -> erlang:list_to_float( "0." ++ lists:flatten([erlang:integer_to_list(X) || X <- sequence_loop(N, Base)]) ).

task() -> [task(X) || X <- lists:seq(2, 5)].



sequence_loop( 0, _Base ) -> [];
sequence_loop( N, Base ) ->
	New_n = N div Base,
	Digit = N rem Base,
	[Digit | sequence_loop( New_n, Base )].

task( Base ) ->
	io:fwrite( "Base ~p:", [Base] ),
	[io:fwrite( " ~p", [sequence(X, Base)] ) || X <- lists:seq(0, 9)],
	io:fwrite( "~n" ).
