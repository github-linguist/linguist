-module( largest_int_from_concatenated ).

-export( [ints/1, task/0] ).

ints( Ints ) ->
	Int_strings = [erlang:integer_to_list(X) || X <- Ints],
	Pad_ints = [{X ++ X, X} || X <- Int_strings],
	erlang:list_to_integer( lists:append([Int || {_Pad, Int} <- lists:reverse(lists:sort(Pad_ints))]) ).

task() ->
	[io:fwrite("Largest ~p from ~p~n", [ints(X), X]) || X <- [[1, 34, 3, 98, 9, 76, 45, 4], [54, 546, 548, 60]]].
