-module( one_of_n_lines_in_file  ).

-export( [one_of_n/1, task/0] ).

one_of_n( N ) -> loop( N, 2, random:uniform(), 1 ).

task() ->
	Dict = lists:foldl( fun update_counter/2,  dict:new(), lists:seq(1, 1000000) ),
	[io:fwrite("Line ~p selected: ~p~n", [X, dict:fetch(X, Dict)]) || X <- lists:sort(dict:fetch_keys(Dict))].


loop( Max, N, _Random, Acc ) when N =:= Max + 1 -> Acc;
loop( Max, N, Random, _Acc ) when Random < (1/N) -> loop( Max, N + 1, random:uniform(), N );
loop( Max, N, _Random, Acc ) -> loop( Max, N + 1, random:uniform(), Acc ).

update_counter( _N, Dict ) -> dict:update_counter( one_of_n(10), 1, Dict ).
