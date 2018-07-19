-module( atomic_updates ).
-export( [buckets/1, buckets_get/2, buckets_get_all/1, buckets_move_contents/4, task/0] ).

buckets( N ) ->
	Buckets = erlang:list_to_tuple( lists:seq(1, N) ),
	erlang:spawn_link( fun() -> buckets_loop(Buckets) end ).

buckets_get( N, Buckets_pid ) ->
	{is_buckets_alive, true} = {is_buckets_alive, erlang:is_process_alive( Buckets_pid )},
	Buckets_pid ! {get, N, erlang:self()},
	receive
	{value, Buckets_pid, Value} -> Value
	end.

buckets_get_all( Buckets_pid ) ->
	{is_buckets_alive, true} = {is_buckets_alive, erlang:is_process_alive( Buckets_pid )},
	Buckets_pid ! {get_all, erlang:self()},
	receive
	{values, Buckets_pid, Values} -> Values
	end.

buckets_move_contents( Amount, From, To, Buckets_pid ) ->
	{is_buckets_alive, true} = {is_buckets_alive, erlang:is_process_alive( Buckets_pid )},
	Buckets_pid ! {move_contents, Amount, From, To, erlang:self()},
	receive
	{move_contents_done, Buckets_pid} -> ok
	end.

task() ->
	erlang:spawn( fun() ->
		N = 10,
		Buckets = buckets( N ),
		erlang:spawn_link( fun() -> closer_loop(N, Buckets) end ),
		erlang:spawn_link( fun() -> redistribute_loop(N, Buckets) end ),
		display_loop( 0, N, Buckets ),
		erlang:exit( stop )
	end ).



closer_loop( N, Buckets ) ->
	One = random:uniform( N ),
	Two = random:uniform( N ),
	Difference = buckets_get( One, Buckets ) - buckets_get( Two, Buckets ),
	{Amount, From, To} = closer_loop_how_to_move( Difference, One, Two ),
	buckets_move_contents( Amount, From, To, Buckets ),
	closer_loop( N, Buckets ).

closer_loop_how_to_move( Difference, One, Two ) when Difference < 0 ->
	{-1* Difference div 2, Two, One};
closer_loop_how_to_move( Difference, One, Two ) ->
	{Difference div 2, One, Two}.

buckets_loop( Buckets ) ->
	receive
	{get, N, Pid} ->
		Pid ! {value, erlang:self(), erlang:element( N, Buckets )},
		buckets_loop( Buckets );
	{get_all, Pid} ->
		Pid ! {values, erlang:self(), erlang:tuple_to_list( Buckets )},
		buckets_loop( Buckets );
	{move_contents, Amount, From, To, Pid} ->
		Pid ! {move_contents_done, erlang:self()},
		buckets_loop( buckets_loop_move_contents(Amount, From, To, Buckets) )
	end.

buckets_loop_move_contents( _Amount, Same, Same, Buckets ) ->
	Buckets;
buckets_loop_move_contents( Amount, From, To, Buckets ) ->
	Amount_from = erlang:element( From, Buckets ),
	Clamped_amount = erlang:min( Amount, Amount_from ),
	Removed = erlang:setelement( From, Buckets, Amount_from - Clamped_amount ),
	Amount_to = erlang:element( To, Buckets ) + Clamped_amount,
	erlang:setelement( To, Removed, Amount_to ).

display_loop( N, N, _Buckets ) -> ok;
display_loop( Counter, N, Buckets ) ->
	Contents = buckets_get_all( Buckets ),
	io:fwrite( "~p = ~p~n", [Contents, lists:sum(Contents)] ),
	timer:sleep( 100 ),
	display_loop( Counter + 1, N, Buckets ).

redistribute_loop( N, Buckets ) ->
	Amount = random:uniform( N ),
	From = random:uniform( N ),
	To = random:uniform( N ),
	buckets_move_contents( Amount, From, To, Buckets ),
	redistribute_loop( N, Buckets ).
