-module( sort_disjoint ).

-export( [sublist/2, task/0] ).

sublist( Values, Indices ) ->
	Sorted_indices = lists:sort( Indices ),
	Values_indexes = lists:seq( 1, erlang:length(Values) ),
	{[], [], Indices_values} = lists:foldl( fun indices_values/2, {Values, Sorted_indices, []}, Values_indexes ),
	Sorted_indices_values = lists:zip( Sorted_indices, lists:sort(Indices_values) ),
	{Sorted_values, {[], []}} = lists:mapfoldl( fun merge/2, {Values, Sorted_indices_values}, Values_indexes ),
	Sorted_values.

task() -> sublist( [7, 6, 5, 4, 3, 2, 1, 0], [7, 2, 8] ).



indices_values( Index, {[H | Values], [Index | Indices], Indices_values} ) -> {Values, Indices, [H | Indices_values]};
indices_values( _Index, {[_H | Values], Indices, Indices_values} ) -> {Values, Indices, Indices_values}.

merge( Index, {[_H | Values], [{Index, Value} | Sorted_indices_values]} ) -> {Value, {Values, Sorted_indices_values}};
merge( _Index, {[H | Values], Sorted_indices_values} ) -> {H, {Values, Sorted_indices_values}}.
