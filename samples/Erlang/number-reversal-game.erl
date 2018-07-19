-module( number_reversal_game ).

-export( [task/0, start/1] ).

start( N ) when N > 1 ->
	io:fwrite( "Usage: ~s~n", [usage(N)] ),
	Targets = lists:seq( 1, N ),
	Jumbleds = [X||{_,X} <- lists:sort([ {random:uniform(), Y} || Y <- Targets])],
	Attempt = loop( Targets, Jumbleds, 0 ),
	io:fwrite( "Numbers sorted in ~p atttempts~n", [Attempt] ).

task() -> start( 9 ).



loop( Targets, Targets, Attempt ) -> Attempt;
loop( Targets, Jumbleds, Attempt ) ->
	io:fwrite( "~p~n", [Jumbleds] ),
	{ok,[N]} = io:fread( "How many digits from the left to reverse? ", "~d" ),
	{Lefts, Rights} = lists:split( N, Jumbleds ),
	loop( Targets, lists:reverse(Lefts) ++ Rights, Attempt + 1 ).

usage(N) -> io_lib:format( "Given a jumbled list of the numbers 1 to ~p that are definitely not in ascending order, show the list then ask the player how many digits from the left to reverse. Reverse those digits, then ask again, until all the digits end up in ascending order.", [N] ).
