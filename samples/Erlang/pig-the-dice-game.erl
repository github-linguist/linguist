-module( pig_dice ).

-export( [game/1, goal/0, hold/2, player_name/1, players_totals/1, quit/2, roll/2, score/1, task/0] ).

-record( player, {name, score=0, total=0} ).

game( [_Player | _T]=Players ) ->
    My_pid = erlang:self(),
    erlang:spawn_link( fun() -> random:seed(os:timestamp()), game_loop( [#player{name=X} || X <- Players], 100, My_pid ) end ).

goal() -> 100.

hold( Player, Game ) -> Game ! {next_player, Player}.

players_totals( Game ) -> ask( Game, players_totals ).

player_name( Game ) -> ask( Game, name ).

quit( Player, Game ) -> Game ! {quit, Player}.

roll( Player, Game ) -> Game ! {roll, Player}.

score( Game ) -> ask( Game, score ).

task() ->
    Game = game( ["Player1", "Player2"] ),
    Play = erlang:spawn( fun() -> play_loop( Game ) end ),
    receive
    {pig, Result, Game} ->
    	erlang:exit( Play, kill ),
        task_display( Result ),
	Result
    end.



ask( Game, Question ) ->
    Game ! {Question, erlang:self()},
    receive
    {Question, Answer, Game} -> Answer
    end.

game_loop( [], _Goal, Report_pid ) -> Report_pid ! {pig, game_over_all_quite. erlang:self()};
game_loop( [#player{name=Name}=Player | T]=Players, Goal, Report_pid ) ->
	receive
	{name, Pid} ->
	       Pid ! {name, Player#player.name, erlang:self()},
	       game_loop( Players, Goal, Report_pid );
	{next_player, Name} ->
		New_players = game_loop_next_player( Player#player.total + Player#player.score, Players, Goal, Report_pid ),
		game_loop( New_players, Goal, Report_pid );
       {players_totals, Pid} ->
	       Pid ! {players_totals, [{X#player.name, X#player.total} || X <- Players], erlang:self()},
	       game_loop( Players, Goal, Report_pid );
	{quit, Name} -> game_loop( T, Goal, Report_pid );
	{roll, Name} ->
	       New_players = game_loop_roll( random:uniform(6), Players ),
	       game_loop( New_players, Goal, Report_pid );
	{score, Pid} ->
               Pid ! {score, Player#player.score, erlang:self()},
	       game_loop( Players, Goal, Report_pid )
	end.

game_loop_next_player( Total, [Player | T], Goal, Report_pid ) when Total >= Goal ->
	Report_pid ! {pig, [{X#player.name, X#player.total} || X <- [Player | T]]. erlang:self()},
	[];
game_loop_next_player( Total, [Player | T], _Goal, _Report_pid ) ->
	T ++ [Player#player{score=0, total=Total}].

game_loop_roll( 1, [Player | T] ) -> T ++ [Player#player{score=0}];
game_loop_roll( Score, [#player{score=Old_score}=Player | T] ) -> [Player#player{score=Old_score + Score} | T].

play_loop( Game ) ->
	Name = player_name( Game ),
	io:fwrite( "Currently ~p.~n", [players_totals(Game)] ),
	io:fwrite( "Name ~p.~n", [Name] ),
	roll( Name, Game ),
	Score = score( Game ),
	io:fwrite( "Rolled, score this round ~p.~n", [Score] ),
	play_loop_next( Score, Name, Game ),
	play_loop( Game ).

play_loop_command( {ok, ["y" ++ _T]}, _Name, _Game ) -> ok;
play_loop_command( {ok, ["n" ++ _T]}, Name, Game ) -> hold( Name, Game );
play_loop_command( {ok, ["q" ++ _T]}, Name, Game ) -> quit( Name, Game );
play_loop_command( {ok, _T}, Name, Game ) -> play_loop_command( io:fread("Roll again (y/n/q): ", "~s"), Name, Game ).

play_loop_next( 0, _Name, _Game ) -> io:fwrite( "~nScore 0, next player.~n" );
play_loop_next( _Score, Name, Game ) -> play_loop_command( io:fread("Roll again (y/n/q): ", "~s"), Name, Game ).

task_display( Results ) when is_list(Results) ->
        [{Name, Total} | Rest] = lists:reverse( lists:keysort(2, Results) ),
        io:fwrite( "Winner is ~p with total of ~p~n", [Name, Total] ),
        io:fwrite( "Then follows: " ),
        [io:fwrite("~p with ~p~n", [N, T]) || {N, T} <- Rest];
task_display( Result ) -> io:fwrite( "Result: ~p~n", [Result] ).
