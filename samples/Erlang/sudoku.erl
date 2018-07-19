-module( sudoku ).

-export( [display/1, start/1, solve/1, task/0] ).

display( Grid ) -> [display_row(Y, Grid) || Y <- lists:seq(1, 9)].
%% A known value is {{Column, Row}, Value}
%% Top left corner is {1, 1}, Bottom right corner is {9,9}
start( Knowns ) -> dict:from_list( Knowns ).

solve( Grid ) ->
	Sure = solve_all_sure( Grid ),
	solve_unsure( potentials(Sure), Sure ).

task() ->
	Simple = [{{1, 1}, 3}, {{2, 1}, 9}, {{3, 1},4}, {{6, 1}, 2}, {{7, 1}, 6}, {{8, 1}, 7},
		{{4, 2}, 3}, {{7, 2}, 4},
		{{1, 3}, 5}, {{4, 3}, 6}, {{5, 3}, 9}, {{8, 3}, 2},
		{{2, 4}, 4}, {{3, 4}, 5}, {{7, 4}, 9},
		{{1, 5}, 6}, {{9, 5}, 7},
		{{3, 6}, 7}, {{7, 6}, 5}, {{8, 6}, 8},
		{{2, 7}, 1}, {{5, 7}, 6}, {{6, 7}, 7}, {{9, 7}, 8},
		{{3, 8}, 9}, {{6, 8}, 8},
		{{2, 9}, 2}, {{3, 9}, 6}, {{4, 9}, 4}, {{7, 9}, 7}, {{8, 9}, 3}, {{9, 9}, 5}],
	task( Simple ),
	Difficult = [{{6, 2}, 3}, {{8, 2}, 8}, {{9, 2}, 5},
		{{3, 3}, 1}, {{5, 3}, 2},
		{{4, 4}, 5}, {{6, 4}, 7},
		{{3, 5}, 4}, {{7, 5}, 1},
		{{2, 6}, 9},
		{{1, 7}, 5}, {{8, 7}, 7}, {{9, 7}, 3},
		{{3, 8}, 2}, {{5, 8}, 1},
		{{5, 9}, 4}, {{9, 9}, 9}],
	task( Difficult ).



bt( Grid ) -> bt_reject( is_not_allowed(Grid), Grid ).

bt_accept( true, Board ) -> erlang:throw( {ok, Board} );
bt_accept( false, Grid ) -> bt_loop( potentials_one_position(Grid), Grid ).

bt_loop( {Position, Values}, Grid ) -> [bt( dict:store(Position, X, Grid) ) || X <- Values].

bt_reject( true, _Grid ) -> backtrack;
bt_reject( false, Grid ) -> bt_accept( is_all_correct(Grid), Grid ).

display_row( Row, Grid ) ->
	[display_row_group( X, Row, Grid ) || X <- [1, 4, 7]],
	display_row_nl( Row ).

display_row_group( Start, Row, Grid ) ->
	[io:fwrite(" ~c", [display_value(X, Row, Grid)]) || X <- [Start, Start+1, Start+2]],
	io:fwrite( " " ).

display_row_nl( N ) when N =:= 3; N =:= 6; N =:= 9 -> io:nl(), io:nl();
display_row_nl( _N ) -> io:nl().

display_value( X, Y, Grid ) -> display_value( dict:find({X, Y}, Grid) ).

display_value( error ) -> $.;
display_value( {ok, Value} ) -> Value + $0.

is_all_correct( Grid ) -> dict:size( Grid ) =:= 81.

is_not_allowed( Grid ) ->
	is_not_allowed_rows( Grid )
	orelse is_not_allowed_columns( Grid )
	orelse is_not_allowed_groups( Grid ).

is_not_allowed_columns( Grid ) -> lists:any( fun is_not_allowed_values/1, values_all_columns(Grid) ).

is_not_allowed_groups( Grid ) -> lists:any( fun is_not_allowed_values/1, values_all_groups(Grid) ).

is_not_allowed_rows( Grid ) -> lists:any( fun is_not_allowed_values/1, values_all_rows(Grid) ).

is_not_allowed_values( Values ) -> erlang:length( Values ) =/= erlang:length( lists:usort(Values) ).

group_positions( {X, Y} ) -> [{Colum, Row} || Colum <- group_positions_close(X), Row <- group_positions_close(Y)].

group_positions_close( N ) when N < 4 -> [1,2,3];
group_positions_close( N ) when N < 7 -> [4,5,6];
group_positions_close( _N ) -> [7,8,9].

positions_not_in_grid( Grid ) ->
	Keys = dict:fetch_keys( Grid ),
	[{X, Y} || X <- lists:seq(1, 9), Y <- lists:seq(1, 9), not lists:member({X, Y}, Keys)].

potentials_one_position( Grid ) ->
	[{_Shortest, Position, Values} | _T] = lists:sort( [{erlang:length(Values), Position, Values} || {Position, Values} <- potentials( Grid )] ),
	{Position, Values}.

potentials( Grid ) -> lists:flatten( [potentials(X, Grid)  || X <- positions_not_in_grid(Grid)] ).

potentials( Position, Grid ) ->
	Useds = potentials_used_values( Position, Grid ),
	{Position, [Value || Value <- lists:seq(1, 9) -- Useds]}.

potentials_used_values( {X, Y}, Grid ) ->
	Row_positions = [{Row, Y} || Row <- lists:seq(1, 9), Row =/= X],
	Row_values = potentials_values( Row_positions, Grid ),
	Column_positions = [{X, Column} || Column <- lists:seq(1, 9), Column =/= Y],
	Column_values = potentials_values( Column_positions, Grid ),
	Group_positions = lists:delete( {X, Y}, group_positions({X, Y}) ),
	Group_values = potentials_values( Group_positions, Grid ),
	Row_values ++ Column_values ++ Group_values.

potentials_values( Keys, Grid ) ->
	Row_values_unfiltered = [dict:find(X, Grid) || X <- Keys],
	[Value || {ok, Value} <- Row_values_unfiltered].

values_all_columns( Grid ) -> [values_all_columns(X, Grid) || X <- lists:seq(1, 9)].

values_all_columns( X, Grid ) ->
	Positions = [{X, Y} || Y <- lists:seq(1, 9)],
	potentials_values( Positions, Grid ).

values_all_groups( Grid ) ->
	[G123, G456, G789] = [values_all_groups(X, Grid) || X <- [1, 4, 7]],
	[G1,G2,G3] = G123,
	[G4,G5,G6] = G456,
	[G7,G8,G9] = G789,
	[G1,G2,G3,G4,G5,G6,G7,G8,G9].

values_all_groups( X, Grid ) ->[values_all_groups(X, X_offset, Grid) || X_offset <- [X, X+1, X+2]].

values_all_groups( _X, X_offset, Grid ) ->
	Positions = [{X_offset, Y_offset} || Y_offset <- group_positions_close(X_offset)],
	potentials_values( Positions, Grid ).

values_all_rows( Grid ) ->[values_all_rows(Y, Grid) || Y <- lists:seq(1, 9)].

values_all_rows( Y, Grid ) ->
	Positions = [{X, Y} || X <- lists:seq(1, 9)],
	potentials_values( Positions, Grid ).

solve_all_sure( Grid ) -> solve_all_sure( solve_all_sure_values(Grid), Grid ).

solve_all_sure( [], Grid ) -> Grid;
solve_all_sure( Sures, Grid ) -> solve_all_sure( lists:foldl(fun solve_all_sure_store/2, Grid, Sures) ).

solve_all_sure_values( Grid ) -> [{Position, Value} || {Position, [Value]} <- potentials(Grid)].

solve_all_sure_store( {Position, Value}, Acc ) -> dict:store( Position, Value, Acc ).

solve_unsure( [], Grid ) -> Grid;
solve_unsure( _Potentials, Grid ) ->
    try
    bt( Grid )

    catch
    _:{ok, Board} -> Board

    end.

task( Knowns ) ->
	io:fwrite( "Start~n" ),
	Start = start( Knowns ),
	display( Start ),
	io:fwrite( "Solved~n" ),
	Solved = solve( Start ),
	display( Solved ),
	io:nl().
