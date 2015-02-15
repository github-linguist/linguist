-module( n_queens ).

-export( [display/1, solve/1, task/0] ).

display( Board ) ->
	%% Queens are in the positions in the Board list.
	%% Top left corner is {1, 1}, Bottom right is {N, N}. There is a queen in the max column.
	N = lists:max( [X || {X, _Y} <- Board] ),
	[display_row(Y, N, Board) || Y <- lists:seq(1, N)].

solve( N ) ->
    Positions = [{X, Y} || X <- lists:seq(1, N), Y <- lists:seq(1, N)],
    try
    bt( N, Positions, [] )

    catch
    _:{ok, Board} -> Board

    end.

task() ->
    task( 4 ),
    task( 8 ).



bt( N, Positions, Board ) -> bt_reject( is_not_allowed_queen_placement(N, Board), N, Positions, Board ).

bt_accept( true, _N, _Positions, Board ) -> erlang:throw( {ok, Board} );
bt_accept( false, N, Positions, Board ) -> bt_loop( N, Positions, [], Board ).

bt_loop( _N, [], _Rejects, _Board ) -> failed;
bt_loop( N, [Position | T], Rejects, Board ) ->
	bt( N, T ++ Rejects, [Position | Board] ),
	bt_loop( N, T, [Position | Rejects], Board ).

bt_reject( true, _N, _Positions, _Board ) -> backtrack;
bt_reject( false, N, Positions, Board ) -> bt_accept( is_all_queens(N, Board), N, Positions, Board ).

diagonals( N, {X, Y} ) ->
	D1 = diagonals( N, X + 1, fun diagonals_add1/1, Y + 1, fun diagonals_add1/1 ),
	D2 = diagonals( N, X + 1, fun diagonals_add1/1, Y - 1, fun diagonals_subtract1/1 ),
	D3 = diagonals( N, X - 1, fun diagonals_subtract1/1, Y + 1, fun diagonals_add1/1 ),
	D4 = diagonals( N, X - 1, fun diagonals_subtract1/1, Y - 1, fun diagonals_subtract1/1 ),
	D1 ++ D2 ++ D3 ++ D4.

diagonals( _N, 0, _Change_x, _Y, _Change_y ) -> [];
diagonals( _N, _X, _Change_x, 0, _Change_y ) -> [];
diagonals( N, X, _Change_x, _Y, _Change_y ) when X > N -> [];
diagonals( N, _X, _Change_x, Y, _Change_y ) when Y > N -> [];
diagonals( N, X, Change_x, Y, Change_y ) -> [{X, Y} | diagonals( N, Change_x(X), Change_x, Change_y(Y), Change_y )].

diagonals_add1( N ) -> N + 1.

diagonals_subtract1( N ) -> N - 1.

display_row( Row, N, Board ) ->
	[io:fwrite("~s", [display_queen(X, Row, Board)]) || X <- lists:seq(1, N)],
	io:nl().

display_queen( X, Y, Board ) -> display_queen( lists:member({X, Y}, Board) ).
display_queen( true ) -> " Q";
display_queen( false ) -> " .".

is_all_queens( N, Board ) -> N =:= erlang:length( Board ).

is_diagonal( _N, [] ) -> false;
is_diagonal( N, [Position | T] ) ->
	Diagonals = diagonals( N, Position ),
	T =/= (T -- Diagonals)
	orelse is_diagonal( N, T ).

is_not_allowed_queen_placement( N, Board ) ->
	Pieces = erlang:length( Board ),
	{Xs, Ys} = lists:unzip( Board ),
	Pieces =/= erlang:length( lists:usort(Xs) )
	orelse Pieces =/= erlang:length( lists:usort(Ys) )
	orelse is_diagonal( N, Board ).

task( N ) ->
    io:fwrite( "N = ~p. One solution.~n", [N] ),
    Board = solve( N ),
    display( Board ).
