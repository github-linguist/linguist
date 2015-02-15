-module( optional_parameters ).

-export( [sort/2, task/0] ).

sort( Table, Options ) ->
	Ordering = proplists:get_value( ordering, Options, lexicographic ),
	Column = proplists:get_value( column, Options, 1 ),
	Is_reverse = proplists:get_value( reverse, Options, false ),
	Sorted = sort( Table, Ordering, Column ),
	sorted_reverse( Is_reverse, Sorted ).

task() ->
	io:fwrite( "sort defaults ~p~n", [sort( table(), [])] ),
	io:fwrite( "reverse ~p~n", [sort( table(), [reverse])] ),
	io:fwrite( "sort column 3 ~p~n", [sort( table(), [{column, 3}])] ),
	io:fwrite( "reverse ~p~n", [sort( table(), [{column, 3}, reverse])] ),
	io:fwrite( "sort numeric ~p~n", [sort( table(), [{ordering, numeric}])] ),
	io:fwrite( "reverse ~p~n", [sort( table(), [{ordering, numeric}, reverse])] ).



row_numeric( Tuple ) -> erlang:list_to_tuple( [{erlang:list_to_integer(X), X} || X <- erlang:tuple_to_list(Tuple)] ).

row_remove_numeric( Tuple ) -> erlang:list_to_tuple( [Y || {_X, Y} <- erlang:tuple_to_list(Tuple)] ).

sort( Table, lexicographic, Column ) -> lists:keysort( Column, Table );
sort( Table, numeric, Column ) ->
	Numeric_table = [row_numeric(X) || X <- Table],
	Sorted_numeric = lists:keysort( Column, Numeric_table ),
	[row_remove_numeric(X) || X <- Sorted_numeric].

sorted_reverse( true, Sorted ) -> lists:reverse( Sorted );
sorted_reverse( false, Sorted ) -> Sorted.

table() -> [table_row1(), table_row2(), table_row3()].

table_row1() -> {"123", "456", "0789"}.
table_row2() -> {"456", "0789", "123"}.
table_row3() -> {"0789", "123", "456"}.
