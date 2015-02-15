-module( topswops ).

-export( [get_1_first/1, swap/1, task/0] ).

get_1_first( [1 | _T] ) -> 0;
get_1_first( List ) -> 1 + get_1_first( swap(List) ).

swap( [N | _T]=List ) ->
	{Swaps, Remains} = lists:split( N, List ),
	lists:reverse( Swaps ) ++ Remains.
	
task() ->
	Permutations = [{X, permute:permute(lists:seq(1, X))} || X <- lists:seq(1, 10)],
	Swops = [{N, get_1_first_many(N_permutations)} || {N, N_permutations} <- Permutations],
	Topswops = [{N, lists:max(N_swops)} || {N, N_swops} <- Swops],
	io:fwrite( "N	topswaps~n" ),
	[io:fwrite("~p	~p~n", [N, Max]) || {N, Max} <- Topswops].



get_1_first_many( N_permutations ) -> [get_1_first(X) ||  X <- N_permutations].
