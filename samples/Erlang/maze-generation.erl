-module( maze ).

-export( [cell_accessible_neighbours/1, cell_content/1, cell_content_set/2, cell_pid/3, cell_position/1, display/1, generation/2, stop/1, task/0] ).

-record( maze, {dict, max_x, max_y, start} ).
-record( state, {content=" ", controller, is_dug=false, max_x, max_y, neighbours=[], position, walls=[north, south, east, west], walk_done} ).

cell_accessible_neighbours( Pid ) -> read( Pid, accessible_neighbours ).

cell_content( Pid ) -> read( Pid, content ).

cell_content_set( Pid, Content ) -> Pid ! {content, Content, erlang:self()}.

cell_pid( X, Y, Maze ) -> dict:fetch( {X, Y}, Maze#maze.dict ).

cell_position( Pid ) -> read( Pid, position ).

display( #maze{dict=Dict, max_x=Max_x, max_y=Max_y} ) ->
	Position_pids = dict:to_list( Dict ),
	display( Max_x, Max_y, reads(Position_pids, content), reads(Position_pids, walls) ).

generation( Max_x, Max_y ) ->
       Controller = erlang:self(),
       Position_pids = cells_create( Controller, Max_x, Max_y ),
       Pids = [Y || {_X, Y} <- Position_pids],
       [X ! {position_pids, Position_pids} || X <- Pids],
       {Position, Pid} = lists:nth( random:uniform(Max_x * Max_y), Position_pids ),
       Pid ! {dig, Controller},
       receive
       {dig_done} -> ok
       end,
       #maze{dict=dict:from_list(Position_pids), max_x=Max_x, max_y=Max_y, start=Position}.

stop( #maze{dict=Dict} ) ->
      Controller = erlang:self(),
      Pids = [Y || {_X, Y} <- dict:to_list(Dict)],
      [X ! {stop, Controller} || X <- Pids],
      ok.

task() ->
       Maze = generation( 16, 8 ),
       io:fwrite( "Starting at ~p~n", [Maze#maze.start] ),
       display( Maze ),
       stop( Maze ).



cells_create( Controller, Max_x, Max_y ) -> [{{X, Y}, cell_create(Controller, Max_x, Max_y, {X, Y})} || X <- lists:seq(1, Max_x), Y<- lists:seq(1, Max_y)].

cell_create( Controller, Max_x, Max_y, {X, Y} ) -> erlang:spawn_link( fun() -> random:seed( X*1000, Y*1000, (X+Y)*1000 ), loop( #state{controller=Controller, max_x=Max_x, max_y=Max_y, position={X, Y}} ) end ).

display( Max_x, Max_y, Position_contents, Position_walls ) ->
        All_rows = [display_row( Max_x, Y, Position_contents, Position_walls ) || Y <- lists:seq(Max_y, 1, -1)],
        [io:fwrite("~s+~n~s|~n", [North, West]) || {North, West} <- All_rows],
	io:fwrite("~s+~n", [lists:flatten(lists:duplicate(Max_x, display_row_north(true)))] ).

display_row( Max_x, Y, Position_contents, Position_walls ) ->
	North_wests = [display_row_walls(proplists:get_value({X,Y}, Position_contents), proplists:get_value({X,Y}, Position_walls)) || X <- lists:seq(1, Max_x)],
	North = lists:append( [North || {North, _West} <- North_wests] ),
	West = lists:append( [West || {_X, West} <- North_wests] ),
	{North, West}.

display_row_walls( Content, Walls ) -> {display_row_north( lists:member(north, Walls) ), display_row_west( lists:member(west, Walls), Content )}.

display_row_north( true ) -> "+---";
display_row_north( false ) -> "+   ".

display_row_west( true, Content ) -> "| " ++ Content ++ " ";
display_row_west( false, Content ) -> "  " ++ Content ++ " ".

loop( State ) ->
    receive
    {accessible_neighbours, Pid} ->
    	Pid ! {accessible_neighbours, loop_accessible_neighbours( State#state.neighbours, State#state.walls ), erlang:self()},
        loop( State );
    {content, Pid} ->
    	Pid ! {content, State#state.content, erlang:self()},
        loop( State );
    {content, Content, _Pid} ->
        loop( State#state{content=Content} );
    {dig, Pid} ->
	    Not_dug_neighbours = loop_not_dug( State#state.neighbours ),
	    New_walls = loop_dig( Not_dug_neighbours, lists:delete( loop_wall_from_pid(Pid, State#state.neighbours), State#state.walls), Pid ),
	    loop( State#state{is_dug=true, walls=New_walls, walk_done=Pid} );
    {dig_done} ->
	    Not_dug_neighbours = loop_not_dug( State#state.neighbours ),
	    New_walls = loop_dig( Not_dug_neighbours, State#state.walls, State#state.walk_done ),
	    loop( State#state{walls=New_walls} );
    {is_dug, Pid} ->
    	    Pid ! {is_dug, State#state.is_dug, erlang:self()},
	    loop( State );
    {position, Pid} ->
    	Pid ! {position, State#state.position, erlang:self()},
        loop( State );
    {position_pids, Position_pids} ->
        {_My_position, Neighbours} = lists:foldl( fun loop_neighbours/2, {State#state.position, []}, Position_pids ),
        erlang:garbage_collect(), % Shrink process after using large Pid_positions. For memory starved systems.
        loop( State#state{neighbours=Neighbours} );
    {stop, Controller} when Controller =:= State#state.controller ->
    	   ok;
    {walls, Pid} ->
    	    Pid ! {walls, State#state.walls, erlang:self()},
	    loop( State )
    end.

loop_accessible_neighbours( Neighbours, Walls ) -> [Pid || {Direction, Pid} <- Neighbours, not lists:member(Direction, Walls)].

loop_dig( [], Walls, Pid ) ->
	Pid ! {dig_done},
	Walls;
loop_dig( Not_dug_neighbours, Walls, _Pid ) ->
        {Dig_pid, Dig_direction} = lists:nth( random:uniform(erlang:length(Not_dug_neighbours)), Not_dug_neighbours ),
        Dig_pid ! {dig, erlang:self()},
	lists:delete( Dig_direction, Walls ).

loop_neighbours( {{X, Y}, Pid}, {{X, My_y}, Acc} ) when Y =:= My_y + 1 -> {{X, My_y}, [{north, Pid} | Acc]};
loop_neighbours( {{X, Y}, Pid}, {{X, My_y}, Acc} ) when Y =:= My_y - 1 -> {{X, My_y}, [{south, Pid} | Acc]};
loop_neighbours( {{X, Y}, Pid}, {{My_x, Y}, Acc} ) when X =:= My_x + 1 -> {{My_x, Y}, [{east, Pid} | Acc]};
loop_neighbours( {{X, Y}, Pid}, {{My_x, Y}, Acc} ) when X =:= My_x - 1 -> {{My_x, Y}, [{west, Pid} | Acc]};
loop_neighbours( _Position_pid, Acc ) -> Acc.

loop_not_dug( Neighbours ) ->
	My_pid = erlang:self(),
	[Pid ! {is_dug, My_pid} || {_Direction, Pid} <- Neighbours],
	[{Pid, Direction} || {Direction, Pid} <- Neighbours, not read_receive(Pid, is_dug)].

loop_wall_from_pid( Pid, Neighbours ) -> loop_wall_from_pid_result( lists:keyfind(Pid, 2, Neighbours) ).
loop_wall_from_pid_result( {Direction, _Pid} ) -> Direction;
loop_wall_from_pid_result( false ) -> controller.

read( Pid, Key ) ->
	Pid ! {Key, erlang:self()},
	read_receive( Pid, Key ).

read_receive( Pid, Key ) ->
        receive
        {Key, Value, Pid} -> Value
        end.

reads( Position_pids, Key ) ->
    My_pid = erlang:self(),
    [Pid ! {Key, My_pid} || {_Position, Pid} <- Position_pids],
    [{Position, read_receive(Pid, Key)} || {Position, Pid} <- Position_pids].
