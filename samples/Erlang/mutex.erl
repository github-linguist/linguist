-module( mutex  ).

-export( [task/0] ).

task() ->
	Mutex = erlang:spawn( fun() -> loop() end ),
	[erlang:spawn(fun() -> random:seed( X, 0, 0 ), print(Mutex, X, 3) end) || X <- lists:seq(1, 3)].



loop() ->
	receive
	{acquire, Pid} ->
		Pid ! {access, erlang:self()},
		receive
		{release, Pid} -> loop()
		end
	end.

mutex_acquire( Pid ) ->
	Pid ! {acquire, erlang:self()},
	receive
	{access, Pid} -> ok
	end.

mutex_release( Pid ) -> Pid ! {release, erlang:self()}.

print( _Mutex, _N, 0 ) -> ok;
print( Mutex, N, M ) ->
	timer:sleep( random:uniform(100) ),
	mutex_acquire( Mutex ),
	io:fwrite( "Print ~p: ", [N] ),
	[print_slow(X) || X <- lists:seq(1, 3)],
	io:nl(),
	mutex_release( Mutex ),
	print( Mutex, N, M - 1 ).

print_slow( X ) ->
	io:fwrite( " ~p", [X] ),
	timer:sleep( 100 ).
