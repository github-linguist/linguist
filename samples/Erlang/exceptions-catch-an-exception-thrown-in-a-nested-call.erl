-module( exceptions_catch ).

-export( [task/0] ).

task() -> [foo(X) || X<- lists:seq(1, 2)].



baz( 1 ) -> erlang:throw( u0 );
baz( 2 ) -> erlang:throw( u1 ).

foo( N ) ->
	try
	baz( N )

	catch
	_:u0 -> io:fwrite( "Catched ~p~n", [u0] )

	end.
