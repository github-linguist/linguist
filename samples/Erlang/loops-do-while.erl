do() ->
	do(0).
	
do(0) ->
	io:fwrite( "0 " ),
        do( 1 );
do(N) when N rem 6 =:= 0 ->
	io:format("~w~n", [N]);
do(N) ->
	io:fwrite( "~p ", [N] ),
	do(N+1).
