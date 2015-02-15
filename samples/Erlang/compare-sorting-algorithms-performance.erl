-module( compare_sorting_algorithms ).

-export( [task/0] ).

task() ->
	Ns = [100, 1000, 10000],
	Lists = [{"ones", fun list_of_ones/1, Ns}, {"ranges", fun list_of_ranges/1, Ns}, {"reversed_ranges", fun list_of_reversed_ranges/1, Ns}, {"shuffleds", fun list_of_shuffleds/1, Ns}],
	Sorts = [{bubble_sort, fun bubble_sort:list/1}, {insertion_sort, fun sort:insertion/1}, {iquick_sort, fun quicksort:qsort/1}],
	Results = [time_list(X, Sorts) || X <- Lists],
	[file:write_file(X++".png", egd_chart:graph(Y, [{x_label,  "log N"}, {y_label, "log ms"}])) || {X, Y} <- Results].


list_of_ones( N ) -> [1 || _X <- lists:seq(1, N)].
list_of_ranges( N ) -> [X || X <- lists:seq(1, N)].
list_of_reversed_ranges( N ) -> lists:reverse( list_of_ranges(N) ).
list_of_shuffleds( N ) -> [random:uniform(N) || _X <- lists:seq(1, N)].

time_list( {List, List_fun, Values}, Sorts ) ->
	Results = [{Sort, time_sort(Sort_fun, List_fun, Values)} || {Sort, Sort_fun} <- Sorts],
	{List, Results}.

time_sort( Sort_fun, List_fun, Values ) ->
	[time(Sort_fun, List_fun, X) || X <- Values].

time( Fun, List_fun, N ) ->
	{Time, _Result} = timer:tc( fun() -> Fun( List_fun(N) ) end ),
	{math:log10(N), math:log10(Time)}.
