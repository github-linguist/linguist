-module( pig_dice_player ).

-export( [task/0] ).

task() ->
    Goal = pig_dice:goal(),
    Players_holds = [{"Player"++ erlang:integer_to_list(X), X} || X <- [5, 10, 15, 20]],
    Fun = fun ({Player, Total}, Dict) when Total >= Goal -> dict:update_counter( Player, 1, Dict );
        (_Player_total, Dict) -> Dict
        end,
    Dict = lists:foldl( Fun, dict:new(), task_play(Players_holds, Goal, []) ),
    display( dict:to_list(Dict) ).


display( Results ) ->
        [{Name, Total} | Rest] = lists:reverse( lists:keysort(2, Results) ),
        io:fwrite( "Winner is ~p with total of ~p wins~n", [Name, Total] ),
        io:fwrite( "Then follows: " ),
        [io:fwrite("~p with ~p~n", [N, T]) || {N, T} <- Rest].

is_goal_reached( Score, Player, Goal, Player, Game ) ->
        Score + proplists:get_value(Player, pig_dice:players_totals(Game)) >= Goal;
is_goal_reached( _Score, _Other_player, _Goal, _Player, _Game ) -> false.

loop( Hold, Hold, Goal, Player, Game ) ->
    pig_dice:hold( Player, Game ),
    loop( 1, Hold, Goal, Player, Game );
loop( N, Hold, Goal, Player, Game ) ->
    loop_await_my_turn( pig_dice:player_name(Game), Player, Game ),
    pig_dice:roll( Player, Game ),
    Is_goal_reached = is_goal_reached( pig_dice:score(Game), pig_dice:player_name(Game), Goal, Player, Game ),
    loop_done( Is_goal_reached, N, Hold, Goal, Player, Game ).

loop_await_my_turn( Player, Player, _Game ) -> ok;
loop_await_my_turn( _Other, Player, Game ) -> loop_await_my_turn( pig_dice:player_name(Game), Player, Game ).

loop_done( true, _N, _Hold, _Goal, Player, Game ) -> pig_dice:hold( Player, Game );
loop_done( false, N, Hold, Goal, Player, Game ) -> loop( N + 1, Hold, Goal, Player, Game ).

rotate(	[H | T] ) -> T ++ [H].

task_play( Players_holds, _Goal, Acc ) when erlang:length(Players_holds) =:= erlang:length(Acc) -> lists:flatten( Acc );
task_play( Players_holds, Goal, Acc ) ->
    Results = [task_play_n(Players_holds, Goal) || _X <- lists:seq(1, 100)],
    task_play( rotate(Players_holds), Goal, [Results | Acc] ).

task_play_n( Players_holds, Goal ) ->
    Game = pig_dice:game( [X || {X, _Y} <- Players_holds] ),
    Pids = [erlang:spawn(fun() -> loop(1, Y, Goal, X, Game) end) || {X, Y} <- Players_holds],
    receive
    {pig, Result, Game} ->
	[erlang:exit(X, kill) || X <- [Game | Pids]],
        Result
    end.
