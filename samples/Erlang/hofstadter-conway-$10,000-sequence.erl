-module( hofstadter_conway ).

-export( [sequence/1, sequence_div_n/1, task/0] ).

-record( power_of_2, {div_n=0, max=4, min=2, n=0} ).

sequence( 1 ) -> [1];
sequence( 2 ) -> [1, 1];
sequence( Up_to ) when Up_to >= 3 ->
        From_3 = lists:seq( 3, Up_to ),
        Dict = lists:foldl( fun sequence_dict/2, dict:from_list([{1, 1}, {2, 1}]), From_3 ),
	[1, 1 | [dict:fetch(X, Dict) || X <- From_3]].

sequence_div_n( N ) ->
        Sequence = sequence( N ),
	[{X, Y / X} || {X, Y} <- lists:zip(lists:seq(1, N), Sequence)].

task() ->
       [_First | Rest] = sequence_div_n( erlang:round(math:pow(2, 20)) ),
       {_Power, Powers} = lists:foldl( fun max_between_power_of_2/2, {#power_of_2{}, []}, Rest ),
       [io:fwrite( "Maximum between ~p and ~p is ~p for n=~p~n", [X#power_of_2.min, X#power_of_2.max, X#power_of_2.div_n, X#power_of_2.n]) || X <- Powers].



max_between_power_of_2( {N, _Div_n}, {#power_of_2{max=N}=P, Acc} ) ->
        {#power_of_2{max=N * 2, min=N}, [P | Acc]};
max_between_power_of_2( {N, Larger_div_n}, {#power_of_2{div_n=Div_n}=P, Acc} ) when Larger_div_n > Div_n ->
        {P#power_of_2{n=N, div_n=Larger_div_n}, Acc};
max_between_power_of_2( _, Both ) -> Both.

sequence_dict( Key, Dict ) ->
        Last_number = dict:fetch( Key - 1, Dict ),
        At_begining = dict:fetch( Last_number, Dict ),
        At_end = dict:fetch( Key - Last_number, Dict ),
        dict:store( Key, At_begining + At_end, Dict ).
