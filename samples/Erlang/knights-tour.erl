-module( knights_tour ).

-export( [display/1, solve/1, task/0] ).

display( Moves ) ->
	%% The knigh walks the moves {Position, Step_nr} order.
	%% Top left corner is {$a, 8}, Bottom right is {$h, 1}.
	io:fwrite( "Moves:" ),
	lists:foldl( fun display_moves/2, erlang:length(Moves), lists:keysort(2, Moves) ),
	io:nl(),
	[display_row(Y, Moves) || Y <- lists:seq(8, 1, -1)].

solve( First_square ) ->
    try
    bt_loop( 1, next_moves(First_square), [{First_square, 1}] )

    catch
    _:{ok, Moves} -> Moves

    end.

task() ->
	io:fwrite( "Starting {a, 1}~n" ),
	Moves = solve( {$a, 1} ),
	display( Moves ).



bt( N, Move, Moves ) -> bt_reject( is_not_allowed_knight_move(Move, Moves), N, Move, [{Move, N} | Moves] ).

bt_accept( true, _N, _Move, Moves ) -> erlang:throw( {ok, Moves} );
bt_accept( false, N, Move, Moves ) -> bt_loop( N, next_moves(Move), Moves ).

bt_loop( N, New_moves, Moves ) -> [bt( N+1, X, Moves ) || X <- New_moves].

bt_reject( true, _N, _Move, _Moves ) -> backtrack;
bt_reject( false, N, Move, Moves ) -> bt_accept( is_all_knights(Moves), N, Move, Moves ).

display_moves( {{X, Y}, 1}, Max ) ->
	io:fwrite(" ~p. N~c~p", [1, X, Y]),
	Max;
display_moves( {{X, Y}, Max}, Max ) ->
	io:fwrite(" N~c~p~n", [X, Y]),
	Max;
display_moves( {{X, Y}, Step_nr}, Max ) when Step_nr rem 8 =:= 0 ->
	io:fwrite(" N~c~p~n~p. N~c~p", [X, Y, Step_nr, X, Y]),
	Max;
display_moves( {{X, Y}, Step_nr}, Max ) ->
	io:fwrite(" N~c~p ~p. N~c~p", [X, Y, Step_nr, X, Y]),
	Max.

display_row( Row, Moves ) ->
	[io:fwrite(" ~2b", [proplists:get_value({X, Row}, Moves)]) || X <- [$a, $b, $c, $d, $e, $f, $g, $h]],
	io:nl().

is_all_knights( Moves ) when erlang:length(Moves) =:= 64 -> true;
is_all_knights( _Moves ) -> false.

is_asymetric( Start_column, Start_row, Stop_column, Stop_row ) ->
	erlang:abs( Start_column - Stop_column ) =/= erlang:abs( Start_row - Stop_row ).

is_not_allowed_knight_move( Move, Moves ) ->
	no_such_move =/= proplists:get_value( Move, Moves, no_such_move ).

next_moves( {Column, Row} ) ->
	[{X, Y} || X <- next_moves_column(Column), Y <- next_moves_row(Row), is_asymetric(Column, Row, X, Y)].

next_moves_column( $a ) -> [$b, $c];
next_moves_column( $b ) -> [$a, $c, $d];
next_moves_column( $g ) -> [$e, $f, $h];
next_moves_column( $h ) -> [$g, $f];
next_moves_column( C ) -> [C - 2, C - 1, C + 1, C + 2].

next_moves_row( 1 ) -> [2, 3];
next_moves_row( 2 ) -> [1, 3, 4];
next_moves_row( 7 ) -> [5, 6, 8];
next_moves_row( 8 ) -> [6, 7];
next_moves_row( N ) -> [N - 2, N - 1, N + 1, N + 2].
