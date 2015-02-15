-module( arena_storage_pool ).

-export( [task/0] ).

task() ->
      Pid = erlang:spawn_opt( fun() -> loop([]) end, [{min_heap_size, 10000}] ),
      set( Pid, 1, ett ),
      set( Pid, "kalle", "hobbe" ),
      V1 = get( Pid, 1 ),
      V2 = get( Pid, "kalle" ),
      true = (V1 =:= ett) and (V2	=:= "hobbe"),
      erlang:exit( Pid, normal ).



get( Pid, Key ) ->
     Pid ! {get, Key, erlang:self()},
     receive
	{value, Value, Pid} -> Value
     end.

loop( List ) ->
      receive
	{set, Key, Value} -> loop( [{Key, Value} | proplists:delete(Key, List)] );
	{get, Key, Pid} ->
	      Pid ! {value, proplists:get_value(Key, List), erlang:self()},
	      loop( List )
	end.

set( Pid, Key, Value ) -> Pid ! {set, Key, Value}.
