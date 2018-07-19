-module( two_dimensional_array ).

-export( [create/2, get/3, set/4, task/0] ).

create( X, Y ) -> array:new( [{size, X}, {default, array:new( [{size, Y}] )}] ).

get( X, Y, Array ) -> array:get( Y, array:get(X, Array) ).

set( X, Y, Value, Array ) ->
	Y_array = array:get( X, Array ),
	New_y_array = array:set( Y, Value, Y_array ),
	array:set( X, New_y_array, Array ).

task() ->
	{ok, [X, Y]} = io:fread( "Input two integers.  Space delimited, please:  ", "~d ~d" ),
	Array = create( X, Y ),
	New_array = set( X - 1, Y - 1, X * Y, Array ),
	io:fwrite( "In position ~p ~p we have ~p~n", [X - 1, Y - 1, get( X - 1, Y - 1, New_array)] ).
