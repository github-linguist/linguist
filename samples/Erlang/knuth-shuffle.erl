-module( knuth_shuffle ).

-export( [list/1] ).

list( Inputs ) ->
	N = erlang:length( Inputs ),
	{[], Acc} = lists:foldl( fun random_move/2, {Inputs, []}, lists:reverse(lists:seq(1, N)) ),
	Acc.



random_move( N, {Inputs, Acc} ) ->
	Item = lists:nth( random:uniform(N), Inputs ),
	{lists:delete(Item, Inputs), [Item | Acc]}.
