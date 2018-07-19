-module( langtons_ant ).

-export( [task/0] ).

-record( neighbour, {north, south, east, west} ).
-record( state, {colour=white, controller, max_x, max_y, neighbour, position} ).

task() ->
       Controller = erlang:self(),
       Max_x = Max_y = 100,
       Pid_positions = plane_create( Controller, Max_x, Max_y ),
       Pids = [X || {X, _} <- Pid_positions],
       [X ! {pid_positions, Pid_positions} || X <- Pids],
       {Pid, _Position} = lists:keyfind( {Max_x div 2, Max_y div 2}, 2, Pid_positions ),
       Pid ! {ant_start, north, Controller},
       receive
       {ant_arrives, _Pid} -> ok
       end,
       display( Controller, Max_x, Max_y, Pids ),
       [X ! {stop, Controller} || X <- Pids].



display( Controller, Max_x, Max_y, Pids ) ->
        Positions_colours = display_positions_colours( Pids, Controller ),
        All_lines = [display_line( Max_x, Positions_colours, Y ) || Y <- lists:seq(Max_y, 1, -1)],
        Lines_with_black = [X || X <- All_lines, lists:member(black, X)],
        [io:fwrite( "~s~n", [[display_on_screen(X) || X <- Lines]] ) || Lines <- Lines_with_black].

display_line( Max_x, Positions_colours, Y ) -> [proplists:get_value({X,Y}, Positions_colours, white) || X <- lists:seq(1, Max_x)].

display_on_screen( white ) -> $_;
display_on_screen( black ) -> $#.

display_positions_colours( Pids, Controller ) ->
        [X ! {position_colour, Controller} || X <- Pids],
        [display_positions_colours_receive() || _X <- Pids].

display_positions_colours_receive( ) ->
        receive
        {position_colour, Position, Colour} -> {Position, Colour}
        end.

loop( State ) ->
    receive
    {pid_positions, Pid_positions} ->
        {_My_position, Neighbour} = lists:foldl( fun loop_neighbour/2, {State#state.position, #neighbour{}}, Pid_positions ),
        erlang:garbage_collect(), % Shrink process after using large Pid_positions. For memory starved systems.
        loop( State#state{neighbour=Neighbour} );
    {ant_start, Direction, Controller} when Controller =:= State#state.controller ->
                {Pid, New_state} = loop_ant_departs( Direction, State ),
                Pid ! {ant_arrives, erlang:self()},
                loop( New_state );
    {ant_arrives, From} ->
                {Direction, New_state} = loop_ant_arrives( From, State ),
                {To, Newest_state} = loop_ant_departs( Direction, New_state ),
                To ! {ant_arrives, erlang:self()},
                loop( Newest_state );
    {position_colour, Controller} when Controller =:= State#state.controller ->
                Controller ! {position_colour, State#state.position, State#state.colour},
                loop( State );
    {stop, Controller} when Controller =:= State#state.controller -> ok
    end.

loop_ant_arrives( Pid, State ) ->
        Neighbour = State#state.neighbour,
        From = loop_ant_arrives_direction( Pid, Neighbour ),
        {loop_ant_arrives_new_direction(From, State), State}.

loop_ant_arrives_direction( Pid, #neighbour{north=Pid} ) -> north;
loop_ant_arrives_direction( Pid, #neighbour{south=Pid} ) -> south;
loop_ant_arrives_direction( Pid, #neighbour{east=Pid} ) -> east;
loop_ant_arrives_direction( Pid, #neighbour{west=Pid} ) -> west.

loop_ant_arrives_new_direction( north, #state{colour=white} ) -> west;
loop_ant_arrives_new_direction( north, #state{colour=black} ) -> east;
loop_ant_arrives_new_direction( south, #state{colour=white} ) -> east;
loop_ant_arrives_new_direction( south, #state{colour=black} ) -> west;
loop_ant_arrives_new_direction( east, #state{colour=white} ) -> north;
loop_ant_arrives_new_direction( east, #state{colour=black} ) -> south;
loop_ant_arrives_new_direction( west, #state{colour=white} ) -> south;
loop_ant_arrives_new_direction( west, #state{colour=black} ) -> north.

loop_ant_departs( north, #state{position={_X,Y}, max_y=Y}=State ) ->
        {State#state.controller, State};
loop_ant_departs( south, #state{position={_X,1}}=State ) ->
        {State#state.controller, State};
loop_ant_departs( east, #state{position={X,_Y}, max_x=X}=State ) ->
        {State#state.controller, State};
loop_ant_departs( west, #state{position={1,_Y}}=State ) ->
        {State#state.controller, State};
loop_ant_departs( Direction, State ) ->
        Neighbour = State#state.neighbour,
        Pid = loop_ant_departs_pid( Direction, Neighbour ),
        {Pid, State#state{colour=other_colour(State)}}.

loop_ant_departs_pid( north, #neighbour{north=Pid} ) -> Pid;
loop_ant_departs_pid( south, #neighbour{south=Pid} ) -> Pid;
loop_ant_departs_pid( east, #neighbour{east=Pid} ) -> Pid;
loop_ant_departs_pid( west, #neighbour{west=Pid} ) -> Pid.

loop_neighbour( {Pid, {X, Y}}, {{X, My_y}, Neighbour} ) when Y =:= My_y + 1 -> {{X, My_y}, Neighbour#neighbour{north=Pid}};
loop_neighbour( {Pid, {X, Y}}, {{X, My_y}, Neighbour} ) when Y =:= My_y - 1 -> {{X, My_y}, Neighbour#neighbour{south=Pid}};
loop_neighbour( {Pid, {X, Y}}, {{My_x, Y}, Neighbour} ) when X =:= My_x + 1 -> {{My_x, Y}, Neighbour#neighbour{east=Pid}};
loop_neighbour( {Pid, {X, Y}}, {{My_x, Y}, Neighbour} ) when X =:= My_x - 1 -> {{My_x, Y}, Neighbour#neighbour{west=Pid}};
loop_neighbour( _Pid_position, Acc ) -> Acc.

other_colour( #state{colour=white} ) -> black;
other_colour( #state{colour=black} ) -> white.

plane_create( Controller, Max_x, Max_y ) -> [{plane_create_cell(Controller, Max_x, Max_y, {X, Y}), {X,Y}} || X <- lists:seq(1, Max_x), Y<- lists:seq(1, Max_y)].
plane_create_cell( Controller, Max_x, Max_y, Position ) -> erlang:spawn_link( fun() -> loop( #state{controller=Controller, max_x=Max_x, max_y=Max_y, position=Position} ) end ).
