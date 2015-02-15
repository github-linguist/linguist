-module(tic_tac_toe).

-export( [task/0] ).

task() -> io:fwrite( "Result: ~p.~n", [turn(player(random:uniform()), board())] ).



board() -> [{X, erlang:integer_to_list(X)} || X <- lists:seq(1, 9)].

board_tuples( Selections, Board ) -> [lists:keyfind(X, 1, Board) || X <- Selections].

computer_move( Player, Board ) ->
	[N | _T] = lists:flatten( [X(Player, Board) || X <- [fun computer_move_win/2, fun computer_move_block/2, fun computer_move_middle/2, fun computer_move_random/2]] ),
	N.

computer_move_block( Player, Board ) ->	computer_move_two_same_player( player(false, Player), Board ).

computer_move_middle( _Player, Board ) ->
	{5, Y} = lists:keyfind( 5, 1, Board ),
	computer_move_middle( is_empty(Y) ).

computer_move_middle( true ) -> [5];
computer_move_middle( false ) -> [].

computer_move_random( _Player, Board ) ->
	Ns = [X || {X, Y} <- Board, is_empty(Y)],
	[lists:nth( random:uniform(erlang:length(Ns)), Ns )].

computer_move_two_same_player( Player, Board ) ->
        Selections = [X || X <- three_in_row_all(), is_two_same_player(Player, X, Board)],
	computer_move_two_same_player( Player, Board, Selections ).

computer_move_two_same_player( _Player, _Board, [] ) -> [];
computer_move_two_same_player( _Player, Board, [Selection | _T] ) -> [X || {X, Y} <- board_tuples(Selection, Board), is_empty(Y)].

computer_move_win( Player, Board ) -> computer_move_two_same_player( Player, Board ).

is_empty( Square ) -> Square =< "9". % Do not use < "10".

is_finished( Board ) -> is_full( Board ) orelse is_three_in_row( Board ).

is_full( Board ) -> [] =:= [X || {X, Y} <- Board, is_empty(Y)].

is_three_in_row( Board ) ->
	Fun = fun(Selections) -> is_three_in_row_same_player( board_tuples(Selections, Board) ) end,
	lists:any( Fun, three_in_row_all() ).

is_three_in_row_same_player( Selected ) -> three_in_row_player( Selected ) =/= no_player.

is_two_same_player( Player, Selections, Board ) -> is_two_same_player( Player, [{X, Y} || {X, Y} <- board_tuples(Selections, Board), not is_empty(Y)] ).

is_two_same_player( Player, [{_X, Player}, {_Y, Player}] ) -> true;
is_two_same_player( _Player, _Selected ) -> false.

player( Random ) when Random > 0.5 -> "O";
player( _Random ) -> "X".

player( true, _Player ) -> finished;
player( false, "X" ) -> "O";
player( false, "O" ) -> "X".

result( Board ) -> result( is_full(Board), Board ).

result( true, _Board ) -> draw;
result( false, Board ) ->
	[Winners] = [Selections || Selections <- three_in_row_all(), three_in_row_player(board_tuples(Selections, Board)) =/= no_player],
	"Winner is " ++ three_in_row_player( board_tuples(Winners, Board) ).

three_in_row_all() -> three_in_row_horisontal() ++ three_in_row_vertical() ++ three_in_row_diagonal().
three_in_row_diagonal() -> [[1,5,9], [3,5,7]].
three_in_row_horisontal() -> [[1,2,3], [4,5,6], [7,8,9]].
three_in_row_vertical() -> [[1,4,7], [2,5,8], [3,6,9]].

three_in_row_player( [{_X, Player}, {_Y, Player}, {_Z, Player}] ) -> three_in_row_player( not is_empty(Player), Player );
three_in_row_player( _Selected ) -> no_player.

three_in_row_player( true, Player ) -> Player;
three_in_row_player( false, _Player ) -> no_player.

turn( finished, Board ) -> result( Board );
turn( "X"=Player, Board ) ->
    N = computer_move( Player, Board ),
    io:fwrite( "Computer, ~p, selected ~p~n", [Player, N] ),
    New_board = [{N, Player} | lists:keydelete(N, 1, Board)],
    turn( player(is_finished(New_board), Player), New_board );
turn( "O"=Player, Board ) ->
    [turn_board_write_horisontal(X, Board) || X <- three_in_row_horisontal()],
    Ns = [X || {X, Y} <- Board, is_empty(Y)],
    Prompt = lists:flatten( io_lib:format("Player, ~p, select one of ~p: ", [Player, Ns]) ),
    N = turn_next_move( Prompt, Ns ),
    New_board = [{N, Player} | lists:keydelete(N, 1, Board)],
    turn( player(is_finished(New_board), Player), New_board ).

turn_board_write_horisontal( Selections, Board ) ->
	Tuples = [lists:keyfind(X, 1, Board) || X <- Selections],
	[io:fwrite( "~p ", [Y]) || {_X, Y} <- Tuples],
	io:fwrite( "~n" ).

turn_next_move( Prompt, Ns ) ->
	{ok,[N]} = io:fread( Prompt, "~d" ),
	turn_next_move_ok( lists:member(N, Ns), Prompt, Ns, N ).

turn_next_move_ok( true, _Prompt, _Ns, N ) -> N;
turn_next_move_ok( false, Prompt, Ns, _N ) -> turn_next_move( Prompt, Ns ).
