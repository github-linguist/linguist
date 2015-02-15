-module( trabb_pardo_knuth ).

-export( [task/0] ).

task() ->
	Sequence = get_11_numbers(),
	S = lists:reverse( Sequence ),
	[perform_operation( fun  function/1, 400, X) || X <- S].


alert( N ) -> io:fwrite( "Operation on ~p overflowed~n", [N] ).

get_11_numbers() ->
	{ok, Ns} = io:fread( "Input 11 integers.  Space delimited, please:  ", "~d ~d ~d ~d ~d ~d ~d  ~d ~d ~d ~d" ),
	11 = erlang:length( Ns ),
	Ns.

function( X ) -> math:sqrt( erlang:abs(X) ) + 5 * math:pow( X, 3 ).

perform_operation( Fun, Overflow, N ) -> perform_operation_check_overflow( N, Fun(N), Overflow ).

perform_operation_check_overflow( N, Result, Overflow ) when Result > Overflow -> alert( N );
perform_operation_check_overflow( N, Result, _Overflow ) -> io:fwrite( "f(~p) => ~p~n", [N, Result] ).
