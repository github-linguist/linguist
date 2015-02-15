-module( mode ).
-export( [example/0, values/1] ).

example() ->
	Set = [1, 2, "qwe", "asd", 1, 2, "qwe", "asd", 2, "qwe"],
	io:fwrite( "In ~p the mode(s) is(are): ~p~n", [Set, values(Set)] ).

values( Set ) ->
	Dict = lists:foldl( fun values_count/2, dict:new(), Set ),
	[X || {X, _Y} <- dict:fold( fun keep_maxs/3, [{0, 0}], Dict )].



keep_maxs( Key, Value, [{_Max_key, Max_value} | _] ) when Value > Max_value ->
	[{Key, Value}];
keep_maxs( Key, Value, [{_Max_key, Max_value} | _]=Maxs ) when Value =:= Max_value ->
	[{Key, Value} | Maxs];
keep_maxs( _Key, _Value, Maxs ) ->
	Maxs.

values_count( Value, Dict ) -> dict:update_counter( Value, 1, Dict ).
